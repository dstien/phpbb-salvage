open System
open System.Linq

open Types
open Collections

let ReadFiles (pattern : string) (parser : Context -> Context) (ctx : Context) =
    let dir =
        match ctx.Config.Input.Value with
        | Input.Directory dir -> dir
        | _ -> failwith "ReadFiles called with no input directory set."

    if ctx.Config.Verbosity > 0 then
        printfn "Reading data directory \"%s\" for file type \"%s\"..." dir pattern

    IO.DirectoryInfo(dir)
        .GetFiles()
        .Where(fun f -> f.Name.Contains(pattern))
        .Where(fun f -> not (f.Name.Contains("login.php")))
        .OrderByDescending(fun f -> f.CreationTime)
        .Select(fun f-> f.FullName, f.CreationTime)
        .ToArray()
    |> Array.fold (
        fun ctx' file ->
            parser (
                Util.ReadFile
                    {
                        ctx' with
                            File = Some (fst file)
                            Timestamp = snd file
                    }
            )
        ) ctx

let ReadDir (ctx : Context) =
    ctx
    |> ReadFiles "index.php"      Parsers.Index.Parse
    |> ReadFiles "viewforum.php"  Parsers.Forum.Parse
    |> ReadFiles "viewtopic.php"  Parsers.Topic.Parse
    |> ReadFiles "memberlist.php" Parsers.Memberlist.Parse
    |> ReadFiles "profile.php"    Parsers.Profile.Parse

[<EntryPoint>]
let main argv =
    let config = Util.ParseArgs (List.ofArray argv) { Input = None; Verbosity = 1; Error = false }
    let ctx = Context.Init config

    if config.Error || config.Input = None then
        printfn "Usage: %s [-v] [-q] [-f sourcetype inputfile] [datadir]" AppDomain.CurrentDomain.FriendlyName
        printfn ""
        printfn "Supply one input type: datadir or -f type file"
        printfn ""
        printfn "  -f  Single file input for given source of type"
        printfn "      Index, Forum, Topic, Memberlist or Profile"
        printfn "  -v  Increase verbosity"
        printfn "  -q  Quiet"
        printfn ""
        1
    else
        let result =
            match config.Input.Value with
            | Directory _ -> ReadDir ctx
            | File (typ, file) ->
                let parser =
                    match typ with
                    | SourceType.Index      -> Parsers.Index.Parse
                    | SourceType.Forum      -> Parsers.Forum.Parse
                    | SourceType.Topic      -> Parsers.Topic.Parse
                    | SourceType.Memberlist -> Parsers.Memberlist.Parse
                    | SourceType.Profile    -> Parsers.Profile.Parse

                parser (Util.ReadFileSingle { ctx with File = Some file })

        match config.Verbosity with
        | 0 -> ()
        | 1 ->
            Util.PrintMapSummary result.Users  "Users"
            Util.PrintMapSummary result.Forums "Forums"
            Util.PrintMapSummary result.Topics "Topics"
            Util.PrintMapSummary result.Posts  "Posts"
        | _ ->
            Util.PrintMap result.Users  "Users"
            Util.PrintMap result.Forums "Forums"
            Util.PrintMap result.Topics "Topics"
            Util.PrintMap result.Posts  "Posts"

        0
