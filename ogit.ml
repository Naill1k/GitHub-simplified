open Arg
open Commands

let usage_msg = "Usage: ./ogit -'fonction de commands.ml'"
let input_files = ref []

let () = Sys.chdir "../../repo" (* On se place dans le répertoire repo *)

let speclist = [("-init", Unit (fun () -> Commands.ogit_init()), ": Execute la commande ogit_init");
                ("-commit", String (fun s -> Commands.ogit_commit s), ": Execute la commande ogit_commit");
                ("-checkout", String (fun s -> Commands.ogit_checkout s), ": Execute la commande ogit_checkout");
                ("-log", Unit (fun () -> Commands.ogit_log()), ": Execute la commande ogit_log");
                ("-merge", String (fun s -> Commands.ogit_merge s), ": Execute la commande ogit_merge")]

let () =
    Arg.parse speclist (fun filename -> input_files := filename::!input_files) usage_msg;
    if !input_files <> [] then
        Format.print_string usage_msg;exit(1)