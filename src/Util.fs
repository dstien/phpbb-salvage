module Util

open System
open System.Text.RegularExpressions
open FSharp.Data
open MBrace.FsPickler
open MBrace.FsPickler.Json

open Types

// Parse config from command line arguments.
let rec ParseArgs argv (config : Config) =
    let err = { config with Error = true }

    match argv with
    | "-v"::xs -> ParseArgs xs { config with Verbosity = config.Verbosity + 1 }
    | "-q"::xs -> ParseArgs xs { config with Verbosity = 0 }
    | "-f"::xs ->
        // Set file input or error if input has already been set.
        match config.Input with
        | None ->
            match xs with
            | typ::file::xss ->
                match typ.ToLower() with
                | "index"      -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Index,      file)) }
                | "forum"      -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Forum,      file)) }
                | "topic"      -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Topic,      file)) }
                | "memberlist" -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Memberlist, file)) }
                | "profile"    -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Profile,    file)) }
                | _ -> err
            | _ -> err
        | _ -> err
    | "-l"::xs ->
        match config.Input with
        | None ->
            match xs with
            | typ::xss ->
                match typ.ToLower() with
                | "json" ->
                    match xss with
                    | file::xsss -> ParseArgs xsss { config with Input = Some (Input.Json file) }
                    | _ -> err
                | "bin" ->
                    match xss with
                    | file::xsss -> ParseArgs xsss { config with Input = Some (Input.Bin file) }
                    | _ -> err
                | _ -> err
            | _ -> err
        | _ -> err
    | "-s"::xs ->
        match xs with
        | typ::xss ->
            match typ.ToLower() with
            | "terminal" -> ParseArgs xss { config with Output = Terminal }
            | "sql" ->
                match xss with
                | file::xsss -> ParseArgs xsss { config with Output = Output.Sql file }
                | _ -> err
            | "json" ->
                match xss with
                | file::xsss -> ParseArgs xsss { config with Output = Output.Json file }
                | _ -> err
            | "bin" ->
                match xss with
                | file::xsss -> ParseArgs xsss { config with Output = Output.Bin file }
                | _ -> err
            | _ -> err
        | _ -> { config with Error = true }
    | "-?"::xs -> { config with Error = true }
    | str::xs ->
        // Set directory input or error if input has already been set.
        match config.Input with
        | None -> ParseArgs xs { config with Input = Some (Input.Directory str) }
        | _ -> { config with Error = true }
    | [] ->
        // Reached end. Error if input wasn't set.
        match config.Input with
        | None -> { config with Error = true }
        | _ -> config

// Remove unnecessary data from context before serializing.
let internal exportableContext (ctx : Context) =
    { ctx with
        File      = None
        Timestamp = DateTime.MinValue
        Html      = HtmlNode.NewText ""
    }

// Save context to JSON for convenient loading of already parsed data.
let SaveJson (file : string) (ctx : Context) =
    use stream = new IO.FileStream(file, IO.FileMode.Create)
    FsPickler.CreateJsonSerializer(true).Serialize(stream, exportableContext ctx)

// Read already parsed data from JSON dump.
let ReadJson (file : string) =
    use stream = new IO.FileStream(file, IO.FileMode.Open)
    FsPickler.CreateJsonSerializer().Deserialize<Context>(stream)

// Save context to binary for efficient loading of already parsed data.
let SaveBin (file : string) (ctx : Context) =
    use stream = new IO.FileStream(file, IO.FileMode.Create)
    FsPickler.CreateBinarySerializer().Serialize(stream, exportableContext ctx)

// Read already parsed data from binary dump.
let ReadBin (file : string) =
    use stream = new IO.FileStream(file, IO.FileMode.Open)
    FsPickler.CreateBinarySerializer().Deserialize<Context>(stream)

// Read file into HtmlDocument.
let ReadFile (ctx : Context) =
    if ctx.Config.Verbosity > 0 then
        printfn "Reading %s (%s)" ctx.File.Value (ctx.Timestamp.ToString())

    // Read entire file and remove all newlines. phpbb have inserted <br/> for every newline in post bodies which FSharp.Data substitutes back to newline.
    let src = IO.File.ReadAllText(ctx.File.Value).Replace("\n", "")
    let doc = HtmlDocument.Load(new IO.StringReader(src))

    { ctx with Html = doc.Html() }

// Read file that hasn't been fed timestamp from directory listing.
let ReadFileSingle (ctx : Context) =
    ReadFile { ctx with Timestamp = IO.File.GetCreationTime(ctx.File.Value) }

// UserType field to string.
let PrintUserType (user : UserType) =
    match user with
    | UserType.Guest name    -> sprintf "\"%s\" (guest)" name
    | UserType.Registered id -> sprintf "%d" id
    | UserType.Unknown       -> "(unknown)"

// Print items in a Map<int, 'T> one by one.
let PrintMap (map' : Map<int, 'T>) (label : string) =
    printfn "%s =" label
    for item in map' do
        printfn "%A" item

// Print summary of a Map<int, 'T>.
let PrintMapSummary (map' : Map<int, 'T>) (label : string) =
    let firstId = (Seq.head map').Key
    let lastId = (map' |> Seq.rev |> Seq.head).Key
    printfn "%s: %d (%d - %d)" label map'.Count firstId lastId

// Get a numeric field value from query string.
let NumericQueryField (link : HtmlNode) (field : string) =
    Int32.Parse(Regex.Match(link.AttributeValue("href"), @"(\?|&)" + field + @"=(\d+)").Groups.[2].Value)

// Parse forum timestamp string relative to source time.
let ParseForumTimestamp (text : string) (sourceTime : DateTime) =
    let formatDate (date : DateTime) = date.ToUniversalTime().ToString("dd MMM yyyy")

    let adjustedText =
        match text with
        | t when t.StartsWith("Today") -> text.Replace("Today", formatDate sourceTime)
        | t when t.StartsWith("Yesterday") -> text.Replace("Yesterday", formatDate (sourceTime.AddDays(-1.0)))
        | t -> t

    DateTime.SpecifyKind(DateTime.Parse(adjustedText), DateTimeKind.Utc)

// Get the previous source by date, regardless of type.
let PreviousSourceOfAny (sources : Map<SourceType, DateTime>) =
    sources
    |> Map.toList
    |> List.map snd
    |> List.sortDescending
    |> List.head

// Get the previous source of given types, default to DateTime.MinValue.
let PreviousSourceOfTypes (sources : Map<SourceType, DateTime>) (types : SourceType list) =
    sources
    |> Map.filter (fun s _ -> types |> List.contains s)
    |> Map.toList
    |> List.map snd
    |> List.sortDescending
    |> List.tryHead
    |> Option.defaultValue DateTime.MinValue
