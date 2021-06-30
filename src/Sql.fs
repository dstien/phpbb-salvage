module Sql

open System

open Types
open Collections

let unixTime (time : DateTime) =
    DateTimeOffset(time).ToUnixTimeSeconds()

let sqlBool (value : bool) =
    match value with
    | false -> 0
    | true  -> 1

let internal sqlString (str : string) =
    sprintf "'%s'" (str.Replace("'", "''").Replace("\n", "'||chr(10)||'"))

let internal sqlStringOption (str : string option) =
    sqlString (
        match str with
        | None -> ""
        | Some str' -> str'
    )

let internal writeValues (sql : IO.StreamWriter) (rows : string list) =
    rows
    |> List.iteri (fun i r ->
        let values = if i = 0 then "VALUES" else ""
        let delim  = if i = rows.Length - 1 then ';' else ','
        sql.WriteLine(sprintf "%-7s(%s)%c" values r delim)
    )

let internal writeUsers (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated users.
INSERT INTO phpbb_users
       (user_id, user_type, group_id, user_new, user_regdate, username        , username_clean  ,  user_passchg, user_lastvisit, user_lastmark, user_lastpost_time, user_posts, user_lang, user_rank, user_allow_viewemail, user_avatar_type       , user_avatar, user_sig)")
   
    let typeNormal = 0
    let typeFounder = 3

    let rows =
        ctx.Users
        |> Seq.take 25
        |> Seq.map (fun u' ->
            let u = u'.Value

            let typ =
                match u.Id with
                | 3 -> typeFounder
                | _ -> typeNormal
            let group =
                match u.Rank with
                | "Administrator" -> 5
                | "Moderator"     -> 4
                | "Senior Member" -> 8
                | _ -> 2
            let rank =
                match u.Rank with
                | "Administrator" -> 1
                | "Moderator" -> 2
                | "Senior Member" -> 3
                | _ -> 0
            let avatarType =
                match u.Avatar with
                | None -> ""
                | Some _ -> "avatar.driver.remote"

            let joinDate   = unixTime u.JoinDate
            let lastActive = unixTime u.LastActive

            sprintf "%7d, %9d, %8d, %8d, %12d, %-16s, %-16s, %13d, %14d, %13d, %18d, %10d, %9s, %9d, %20d, %-23s, %s, %s" u.Id typ group 0 joinDate (sqlString u.Name) (sqlString(u.Name.ToLowerInvariant())) joinDate lastActive lastActive lastActive u.PostCount (sqlString "en") rank (sqlBool u.CanEmail) (sqlString avatarType) (sqlStringOption u.Avatar) (sqlStringOption u.Signature)
        )
        |> Seq.toList

    writeValues sql rows

let internal writeUserGroups (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated user memberships.
INSERT INTO phpbb_user_group
       (group_id, user_id, user_pending)")
   
    let groupAdmins = 5
    let groupMods = 4
    let groupSeniors = 8

    let rows =
        ctx.Users
        |> Seq.take 25
        |> Seq.map (fun u' ->
            let u = u'.Value

            // Only special groups. All users are added to the "Registered" group in a separate query.
            (
                match u.Rank with
                | "Administrator" -> [ groupAdmins; groupMods ]
                | "Moderator"     -> [ groupMods ]
                | "Senior Member" -> [ groupSeniors ]
                | _ -> [ ]
            )
            |> Seq.map (fun g ->
                sprintf "%7d, %9d, %12d" g u.Id 0
            )
        )
        |> Seq.collect (id)
        |> Seq.toList

    writeValues sql rows


let internal writeUserProfiles (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated user profile fields.
INSERT INTO phpbb_profile_fields_data
       (user_id, pf_phpbb_title, pf_phpbb_interests, pf_phpbb_occupation, pf_phpbb_location, pf_phpbb_xboxtag, pf_phpbb_website, pf_phpbb_msn, pf_phpbb_yahoo, pf_phpbb_icq)")

    let rows =
        ctx.Users
        |> Seq.take 25
        |> Seq.map (fun u' ->
            let u = u'.Value
            sprintf "%7d, %s, %s, %s, %s, %s, %s, %s, %s, %s" u.Id (sqlString u.CustomRank) (sqlStringOption u.Interests) (sqlStringOption u.Occupation) (sqlStringOption u.Location) (sqlStringOption u.XboxTag) (sqlStringOption u.Homepage) (sqlStringOption u.MSN) (sqlStringOption u.YM) (sqlStringOption u.ICQ)
        )
        |> Seq.toList

    writeValues sql rows

let internal writeForums (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated forums.
INSERT INTO phpbb_forums
       (forum_id, forum_type, forum_flags, left_id, right_id, enable_icons, forum_name, forum_desc)")

    let rows =
        ctx.Forums
        |> Seq.map (fun f' ->
            let f = f'.Value
            sprintf "%8d, %10d, %11d, %7d, %8d, %12d, %s, %s" f.Id 1 48 (f.Id) (f.Id + 1) 0 (sqlString f.Name) (sqlString f.Description)
        )
        |> Seq.toList

    writeValues sql rows

let internal writeTopics (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated topics.
INSERT INTO phpbb_forums
       (topic_id, forum_id, topic_status, topic_type, topic_visibility, topic_posts_approved, topic_views, topic_time, topic_last_post_time, topic_last_view_time, topic_first_post_id, topic_poster, topic_first_poster_name, topic_delete_user, topic_last_post_id, topic_last_poster_id, topic_last_poster_name, topic_title, topic_last_post_subject)")

    let rows =
        ctx.Topics
        |> Seq.take 25
        |> Seq.map (fun t' ->
            let t = t'.Value

            let topicStatus =
                match t.Status with
                | TopicStatus.Locked   -> 1
                | TopicStatus.Unlocked -> 0

            let topicType =
                match t.Type with
                | TopicType.Announcement -> 2
                | TopicType.Sticky       -> 1
                | TopicType.Normal       -> 0

            let firstPost = ctx.Posts.[t.PostIds.Head]
            let lastPost = ctx.Posts.[t.PostIds |> List.sortDescending |> List.head]

            let getUser (post : Post) =
                match post.User with
                | UserType.Registered id -> id, ctx.Users.[id].Name, 0
                | UserType.Guest name -> 0, name, 1
                | UserType.Unknown -> 0, "Unknown", 1

            let userFirstId, userFirstName, userFirstDeleted = getUser firstPost
            let userLastId, userLastName, _ = getUser lastPost

            // Last crawled source of last post is considered last time topic is viewed.
            let lastViewTime = Util.PreviousSourceOfAny lastPost.Sources

            // TODO: Poll

            sprintf "%8d, %8d, %12d, %10d, %16d, %20d, %11d, %10d, %20d, %20d, %19d, %12d, %-23s, %17d, %18d, %20d, %-22s, %s, %s"
                t.Id t.ForumId topicStatus topicType 1 t.PostIds.Length t.Views
                (unixTime firstPost.Timestamp) (unixTime lastPost.Timestamp) (unixTime lastViewTime)
                firstPost.Id userFirstId (sqlString userFirstName) userFirstDeleted
                lastPost.Id userLastId (sqlString userLastName)
                (sqlString t.Title) (sqlString lastPost.Title)
        )
        |> Seq.toList

    writeValues sql rows

let Write (file : string) (ctx : Context) =
    use sql = new IO.StreamWriter(file)

    sql.WriteLine(@"-- Target is phpBB 3.3.4 on PostgreSQL
BEGIN;

--------------------------------------------------
-- USERS
--------------------------------------------------

-- TODO
-- * Reset sequences
-- * Update ACL
-- * Post-process signature BBcode
-- * Import avatars

DO $$
    DECLARE UserIdShift INTEGER = 100000000;
BEGIN
    -- Move the existing users to high ids to avoid collision. Keep special user 1 (guest).
    IF EXISTS (SELECT FROM phpbb_users WHERE user_id >= UserIdShift) THEN
        DELETE FROM phpbb_users
        WHERE  user_id BETWEEN 2 AND (UserIdShift);
    ELSE
        UPDATE phpbb_users
        SET    user_id = user_id + UserIdShift
        WHERE  user_id BETWEEN 2 AND (UserIdShift);
    END IF;

    -- Adjust group memberships accordingly.
    IF EXISTS (SELECT FROM phpbb_user_group WHERE user_id >= UserIdShift) THEN
        DELETE FROM phpbb_user_group
        WHERE  user_id BETWEEN 2 AND (UserIdShift);
    ELSE
        UPDATE phpbb_user_group
        SET    user_id = user_id + UserIdShift
        WHERE  user_id BETWEEN 2 AND (UserIdShift);
    END IF;

    -- Adjust profile fields accordingly.
    IF EXISTS (SELECT FROM phpbb_profile_fields_data WHERE user_id >= UserIdShift) THEN
        DELETE FROM phpbb_profile_fields_data
        WHERE  user_id BETWEEN 2 AND (UserIdShift);
    ELSE
        UPDATE phpbb_profile_fields_data
        SET    user_id = user_id + UserIdShift
        WHERE  user_id BETWEEN 2 AND (UserIdShift);
    END IF;
END $$;")

    writeUsers sql ctx
    writeUserGroups sql ctx
    writeUserProfiles sql ctx

    sql.WriteLine(@"
-- Set all users as members of the REGISTERED group.
INSERT INTO phpbb_user_group
SELECT 2 AS group_id,
       user_id,
	   0 AS group_leader,
	   0 AS user_pending
FROM   phpbb_users
WHERE  user_id BETWEEN 2 AND 100000000;

-- Copy install admin password and permissions to migrated admin account.
UPDATE phpbb_users newadmin
SET    user_permissions = oldadmin.user_permissions,
       user_password = oldadmin.user_password
FROM   phpbb_users oldadmin
WHERE  newadmin.user_id = 3
       AND oldadmin.user_id = 100000002;

-- Clear custom title if it is the same as the rank title.
UPDATE phpbb_profile_fields_data pfnew
SET    pf_phpbb_title = ''
FROM   phpbb_profile_fields_data pfold
       INNER JOIN phpbb_users u ON u.user_id = pfold.user_id
       INNER JOIN LATERAL (
           SELECT rank_title
           FROM   phpbb_ranks
           WHERE  rank_special = 0
                  AND u.user_posts >= rank_min
           ORDER  BY rank_min DESC
           LIMIT  1
       ) AS r ON true
WHERE  pfnew.user_id = pfold.user_id
       AND u.user_type IN (0, 3)
       AND pfold.pf_phpbb_title = r.rank_title;

--------------------------------------------------
-- FORUMS
--------------------------------------------------

-- TODO
-- * Categories
-- * Ordering
-- * Moderators
-- * User tracking timestamp

TRUNCATE TABLE phpbb_forums;")

    writeForums sql ctx

    sql.WriteLine(@"
--------------------------------------------------
-- Topics
--------------------------------------------------

TRUNCATE TABLE phpbb_topics;
TRUNCATE TABLE phpbb_posted;
TRUNCATE TABLE phpbb_track;
TRUNCATE TABLE phpbb_watch;")

    writeTopics sql ctx

    sql.WriteLine(@"
--ROLLBACK;
--COMMIT;
")