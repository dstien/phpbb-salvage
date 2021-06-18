open System
open System.Linq

open Types
open Collections

let ReadFiles (config : Config) (pattern : string) (parser : Config -> string -> unit) =
    if config.Verbosity > 0 then
        printfn "Reading data directory \"%s\"..." config.DataDir

    IO.DirectoryInfo(config.DataDir)
        .GetFiles()
        .Where(fun f -> f.Name.Contains(pattern))
        .Where(fun f -> not (f.Name.Contains("login.php")))
        .OrderByDescending(fun f -> f.CreationTime)
        .Select(fun f-> f.FullName)
        .ToArray()
    |> Array.iter (parser config)

let ReadDir (config : Config) =
    ReadFiles config "viewtopic.php" Parsers.Topic.Parse
    ReadFiles config "viewforum.php" Parsers.Forum.Parse
    ReadFiles config "memberlist.php" Parsers.Memberlist.Parse
    ReadFiles config "profile.php" Parsers.User.Parse

[<EntryPoint>]
let main argv =
    let config = Util.ParseArgs (List.ofArray argv) { DataDir = ""; Verbosity = 1; Error = false }

    if config.Error then
        printfn "Usage: %s [-v] [-q] datadir" AppDomain.CurrentDomain.FriendlyName
        printfn ""
        printfn "  -v  Increase verbosity"
        printfn "  -q  Quiet"
        printfn ""
        1
    else
        ReadDir config

        match config.Verbosity with
        | 0 -> ()
        | 1 ->
            Users.Summary()
            Forums.Summary()
            Topics.Summary()
            Posts.Summary()
        | _ ->
            Users.Print()
            Forums.Print()
            Topics.Print()
            Posts.Print()

        0
