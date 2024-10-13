(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let ogit_init () =
  if Sys.file_exists ".ogit" then
    failwith "Le repertoire .ogit existe"
  else
    let _ = Sys.command "mkdir .ogit" in
    let _ = Sys.command "mkdir .ogit/logs" in
    let _ = Sys.command "mkdir .ogit/objects" in
    let h = Logs.store_commit (Logs.init_commit ()) in
    Logs.set_head [h]


(** Vérifie si il n'y a pas de fichiers finissant par ..cr ou ..cl dans l'arborescence *)
let rec check_conflit rep =
  let contenu = Sys.readdir rep in (* Récupération du contenu du répertoire *)
  for i=0 to (Array.length contenu)-1 do 
    if contenu.(i).[0] <> '.' then begin (* Les fichiers commençant par un point sont ignorés *)
      let file = rep^"/"^contenu.(i) in (* Le nom du fichier avec le chemin à partir de repo *)
      if Sys.is_directory file then
        check_conflit file
      else if (String.ends_with ~suffix:"..cl" file) || (String.ends_with ~suffix:"..cr" file) then
        failwith ("Resoudre les conflits avant de commit : "^file) end
    done


let ogit_commit _msg =
  check_conflit ".";
  let commit = Logs.make_commit _msg (Objects.store_work_directory ()) in 
  let h_commit = Logs.store_commit commit in 
  Logs.set_head [h_commit]


let ogit_checkout _hash =
  if not (Sys.file_exists (".ogit/logs/"^_hash)) then
    raise (Invalid_argument ("Hash logs inconnu : "^_hash))
  else
    let commit = Logs.read_commit (Digest.from_hex _hash) in (* Récupération du commit à partir de son hash *)
    let objet_rep = Objects.read_directory_object commit.content in (* Récupération de l'objet t à partir du commit *)
    Objects.clean_work_directory ();
    Objects.restore_work_directory objet_rep;
    Logs.set_head [Digest.from_hex _hash]


let ogit_log () =
  if not (Sys.file_exists ".ogit") then
    raise (failwith "Le repertoire .ogit n'existe pas")
  else
    (* Parcours l'historique de logs et stocke le nombre d'apparition de chaque log *)
    let rec parcours_logs res = function
      | [] -> res
      | [h] when List.mem_assoc h res -> let commit = Logs.read_commit h in parcours_logs ((h,1+List.assoc h res)::res) commit.parents
      | [h] -> let commit = Logs.read_commit h in parcours_logs ((h,1)::res) commit.parents
      | h::tl -> let tmp = parcours_logs res [h] in parcours_logs tmp tl
    in

    (* Renvoie la liste des logs ancêtres communs de plusieurs branches (logs qui apparaissent plus que leur suivant) *)
    let rec find_splits res = function
      | (h1,n1)::(h2,n2)::tl when n1>n2 -> find_splits (h1::res) ((h2,n2)::tl)
      | (_,_)::(h2,n2)::tl -> find_splits res ((h2,n2)::tl)
      | _ -> res
    in

    (* Affiche la ligne correspondante *)
    let affiche_ligne nb_branch lvl milieu fin =
        for _=1 to (lvl-1) do Format.print_string "|  " done;
        Format.print_string milieu;
        for _=(lvl+1) to nb_branch do Format.print_string "|  " done;
        Format.print_string fin
      in

    let tmp = parcours_logs [] (Logs.get_head()) in
    let splits = ref (find_splits [] tmp) in (* Contient les hashs des logs ancêtres communs de plusieurs branches *)
    let stops = ref [] in (* Contient les séparations qui ont déjà été atteintes *)

    (* Affiche le graphe de l'historique des logs *)   
    let rec iter nb_branch lvl = function 
      | [h] when List.mem h !splits -> begin (* Si le log est à l'origine de plusieurs branches *)
          splits := List.filter (fun el -> el<>h) !splits; (* Ce log est retiré de la liste pour ne pas afficher la séparation à chaque fois *)
          iter (nb_branch-1) lvl [h]; (* On affiche les infos sur ce logs et les précédents *)
          stops := h::!stops; (* Ce log est mémorisé pour ne pas afficher 2 fois les commits précédents *)
          
          affiche_ligne (nb_branch-1) lvl "| \\  " "\n";
        end

      | [h] when not (List.mem h !stops) -> begin
        let commit = Logs.read_commit h in 
        iter nb_branch lvl commit.parents;
        
        affiche_ligne nb_branch lvl "*  " ((Digest.to_hex h)^" : "^commit.message^"\n");
      end

      | [h1;h2] -> begin (* Deux parents (merge) *)
      (* On affiche les infos sur chaque commit et ses ancêtres *)
        iter (nb_branch+1) lvl [h1];
        iter (nb_branch+1) (lvl+1) [h2];
        
        affiche_ligne nb_branch lvl "| /  " "\n";
      end

      | _ -> ()

    in iter 1 1 (Logs.get_head())


(** Vérifie si le hash de log donné n'est pas un état ancêtre de l'état actuel *)
let est_ancetre h_test =
  let rec iter = function
      | [""] -> false
      | h::_ when h=h_test -> true
      | h::tl -> begin
          let parents = String.split_on_char ';' (In_channel.with_open_text (".ogit/logs/"^h) input_line) in 
          iter parents || iter tl
          end
      | _ -> false
  in
  iter (List.map Digest.to_hex (Logs.get_head()))


let ogit_merge _hash =
  if not (Sys.file_exists (".ogit/logs/"^_hash)) then
    raise (Invalid_argument ("Hash logs inconnu : "^_hash))
  else if est_ancetre _hash then
    failwith "Tentative de merge avec un etat ancetre de l'etat actuel"
  else
    check_conflit ".";

    let ic = open_in (".ogit/logs/"^_hash) in 
    let _ = input_line ic in 
    let _ = input_line ic in 
    let _ = input_line ic in 
    let h_repo_r = input_line ic in (* Seul le hash de l'objet répertoire est important *)

    let obj = Objects.read_directory_object (Digest.from_hex h_repo_r) in 
    let conflit = Objects.merge_work_directory_I obj in 
    Logs.set_head (Digest.from_hex _hash::Logs.get_head());
    
    if conflit then begin
      Format.print_string "CONFLICT (object): Merge conflict in\n";
      ignore(Sys.command "find -name '*..c?'");
      Format.print_string ("Automatic merge failed; fix conflicts and then commit the result\n") end
    else
      ogit_commit ("Merge")