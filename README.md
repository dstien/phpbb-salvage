# phpbb-salvage

Partially recreate [phpBB](https://www.phpbb.com/) database from public HTML pages. Board, topic and post links will be retained by keeping their original ids, but private info such as e-mail addresses, passwords and internal messages will not be restored. User accounts must be manually reset by an admin for continued use.

Due to the many customization options, a plethora of styles and long version history of phpBB, a lot of manual adjustments to ``phpbb-salvage`` must be performed to tailor the parsing for the forum you are targeting. The code in this repository was made to restore [forum.mm2c.com](https://forum.mm2c.com/) to phpBB 3.3.8 from a HTML scrape of its phpBB 2.0.11 with a custom style based on "subBlue", custom profile fields, ranks, bbCode and smileys.

## Build
```
dotnet build
```

## Usage
```
phpbb-salvage [-v] [-q] [-s outputtype outputfile] [-l inputtype inputfile] [-f sourcetype sourcefile] [datadir]

Supply one input type: datadir, -f type file or -l type file

  -s  Save parsed data in given format of type
      Terminal (default, no file), SQL, JSON or Bin
  -l  Load previously parsed data in given format of type
      JSON or Bin
  -f  Parse single file input for given source of type
      Index, Forum, Topic, Memberlist or Profile
  -v  Increase verbosity
  -q  Quiet
```

## Manual

### 0. Gather source data
Download forum webpages from a running server or the [Wayback Machine](https://web.archive.org/). From a live server you can get all topic pages and profiles by downloading sequentially by known id ranges. From the Wayback Machine you can get URLs from the [CDX API](https://github.com/internetarchive/wayback/blob/master/wayback-cdx-server/README.md). Be wary of the many unnecessary pages produced by an undirected crawl.

```
# Board index
> echo 'http://myforum/index.php' > urls

# User profiles
> printf 'http://myforum/profile.php?mode=viewprofile&u=%d\n' {2..100} >> urls

# Topics, first page
> printf 'http://myforum/viewtopic.php?t=%d\n' {1..1000} >> urls

# Download
> wget --append-output=log --limit-rate=100k --wait=5 --force-directories --timestamping --input-file=urls

# Topics, next page
> page=2; perpage=15; prevstart=$((perpage*(page-2))); start=$((perpage*(page-1))); rg --files-with-matches "start=$start\">Next</a></b></span></td>\$" | cut -d '&' -f 1 | sort -g -t '=' -k2 | sed -e "s/^/http:\/\/myforum\/
/;s/\$/\&start=$start/"

# Then download and repeat until there are no more topics with more pages left
```

### 1. Parse
Use ``phpbb-salvage`` to parse the datadir. You'd want to begin with individual files to test the parser and print the result in the terminal:
```
phpbb-salvage -v -f Topic 'datadir\viewtopic.php?t=1'
```

When all data can be parsed you can store it in a binary or JSON cache file for fast loading when tweaking the output format:
```
phpbb-salvage -s Bin data.bin datadir
```

Generate SQL from cached parse result:
```
phpbb-salvage -s data.sql -l Bin data.bin
```

### 2. Insert in clean phpBB database
Run the generated SQL file in your favourite SQL client. ``phpbb-salvage`` is written for PostgreSQL, minor tweaks to the output are needed if using other databases.

### 3. Reset users
The admin user must set new e-mail addresses for accounts manually in order to initiate password reset.

## TODO
Suggested improvements for aspiring ``phpbb-salvage`` contributors:
* Automate crawling of topic pages
* Improve parser robustness
* Handle SQL inserts of long posts without truncation
* Import avatar images from files
* Populate dummy poll votes
* Board categories
* Board sort order
* User tracking timestamps (new posts since last activity)
