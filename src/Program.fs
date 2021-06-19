open System
open System.Linq

open Types
open Collections

let ReadFiles (config : Config) (pattern : string) (parser : Config -> string -> unit) =
    let dir =
        match config.Input.Value with
        | Input.Directory dir -> dir
        | _ -> failwith "ReadFiles called with no input directory set."

    if config.Verbosity > 0 then
        printfn "Reading data directory \"%s\" for file type \"%s\"..." dir pattern

    IO.DirectoryInfo(dir)
        .GetFiles()
        .Where(fun f -> f.Name.Contains(pattern))
        .Where(fun f -> not (f.Name.Contains("login.php")))
        .OrderByDescending(fun f -> f.CreationTime)
        .Select(fun f-> f.FullName)
        .ToArray()
    |> Array.iter (parser config)

let ReadDir (config : Config) =
    ReadFiles config "index.php"      Parsers.Index.Parse
    ReadFiles config "viewtopic.php"  Parsers.Topic.Parse
    ReadFiles config "viewforum.php"  Parsers.Forum.Parse
    ReadFiles config "memberlist.php" Parsers.Memberlist.Parse
    ReadFiles config "profile.php"    Parsers.Profile.Parse

[<EntryPoint>]
let main argv =
    let config = Util.ParseArgs (List.ofArray argv) { Input = None; Verbosity = 1; Error = false }

    if config.Error || config.Input = None then
        printfn "Usage: %s [-v] [-q] [-f sourcetype inputfile] [datadir]" AppDomain.CurrentDomain.FriendlyName
        printfn ""
        printfn "Supply one input type, datadir or -f inputfile."
        printfn ""
        printfn "  -f  Single file input for given source of type"
        printfn "      Index, Forum, Topic, Memberlist or Profile"
        printfn "  -v  Increase verbosity"
        printfn "  -q  Quiet"
        printfn ""
        1
    else
        match config.Input.Value with
        | Directory _ -> ReadDir config
        | File (typ, file) ->
            match typ with
            | SourceType.Index      -> Parsers.Index.Parse config file
            | SourceType.Forum      -> Parsers.Forum.Parse config file
            | SourceType.Topic      -> Parsers.Topic.Parse config file
            | SourceType.Memberlist -> Parsers.Memberlist.Parse config file
            | SourceType.Profile    -> Parsers.Profile.Parse config file

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
