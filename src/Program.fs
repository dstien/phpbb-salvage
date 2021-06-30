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
    |> Topics.PostProcess

[<EntryPoint>]
let main argv =
    let config = Util.ParseArgs (List.ofArray argv) { Input = None; Output = Terminal; Verbosity = 1; Error = false }
    let ctx = Context.Init config

    if config.Error || config.Input = None then
        printfn "Usage: %s [-v] [-q] [-s outputtype outputfile] [-l inputtype inputfile] [-f sourcetype sourceile] [datadir]" AppDomain.CurrentDomain.FriendlyName
        printfn ""
        printfn "Supply one input type: datadir, -f type file or -l type file"
        printfn ""
        printfn "  -s  Save parsed data in given format of type"
        printfn "      Terminal (default, no file), SQL, JSON or Bin"
        printfn "  -l  Load previously parsed data in given format of type"
        printfn "      JSON or Bin"
        printfn "  -f  Parse single file input for given source of type"
        printfn "      Index, Forum, Topic, Memberlist or Profile"
        printfn "  -v  Increase verbosity"
        printfn "  -q  Quiet"
        printfn ""
        1
    else
        let result =
            match config.Input.Value with
            | Input.Directory _ -> ReadDir ctx
            | Input.File (typ, file) ->
                let parser =
                    match typ with
                    | SourceType.Index      -> Parsers.Index.Parse
                    | SourceType.Forum      -> Parsers.Forum.Parse
                    | SourceType.Topic      -> Parsers.Topic.Parse
                    | SourceType.Memberlist -> Parsers.Memberlist.Parse
                    | SourceType.Profile    -> Parsers.Profile.Parse

                parser (Util.ReadFileSingle { ctx with File = Some file })
                |> Topics.PostProcess

            | Input.Json file -> Util.ReadJson file
            | Input.Bin  file -> Util.ReadBin  file

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

        match config.Output with
        | Output.Terminal  -> ()
        | Output.Sql  file -> Sql.Write     file result
        | Output.Json file -> Util.SaveJson file result
        | Output.Bin  file -> Util.SaveBin  file result

        0
