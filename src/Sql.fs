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

let internal writeUsers (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated users.
INSERT INTO phpbb_users
       (user_id, user_type, group_id, user_new, user_regdate, username        , username_clean  ,  user_passchg, user_lastvisit, user_lastmark, user_lastpost_time, user_posts, user_lang, user_rank, user_allow_viewemail, user_avatar_type       , user_avatar, user_sig)")
   
    let typeNormal = 0
    let typeFounder = 3

    ctx.Users
    |> Seq.map (fun u -> u.Value)
    |> Seq.take 25
    |> Seq.iteri (fun i u ->
        let values = if i = 0 then "VALUES" else ""
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

        sql.WriteLine(sprintf "%-7s(%7d, %9d, %8d, %8d, %12d, %-16s, %-16s, %13d, %14d, %13d, %18d, %10d, %9s, %9d, %20d, %-23s, %s, %s)," values u.Id typ group 0 joinDate (sqlString u.Name) (sqlString(u.Name.ToLowerInvariant())) joinDate lastActive lastActive lastActive u.PostCount (sqlString "en") rank (sqlBool u.CanEmail) (sqlString avatarType) (sqlStringOption u.Avatar) (sqlStringOption u.Signature))
    )

let internal writeUserGroups (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated user memberships.
INSERT INTO phpbb_user_group
       (group_id, user_id, user_pending)")
   
    let groupAdmins = 5
    let groupMods = 4
    let groupSeniors = 8
    let groupRegistered = 2

    ctx.Users
    |> Seq.map (fun u -> u.Value)
    |> Seq.take 25
    |> Seq.iteri (fun i u ->        
        (
            match u.Rank with
            | "Administrator" -> [ groupAdmins; groupMods; groupRegistered ]
            | "Moderator"     -> [ groupMods; groupRegistered ]
            | "Senior Member" -> [ groupSeniors; groupRegistered ]
            | _ -> [ groupRegistered ]
        )
        |> List.iteri (fun j g ->
            let values = if i = 0 && j = 0 then "VALUES" else ""
            sql.WriteLine(sprintf "%-7s(%7d, %9d, %12d)," values g u.Id 0)
        )
    )

let internal writeUserProfiles (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated user profile fields.
INSERT INTO phpbb_profile_fields_data
       (user_id, pf_phpbb_title, pf_phpbb_interests, pf_phpbb_occupation, pf_phpbb_location, pf_phpbb_xboxtag, pf_phpbb_website, pf_phpbb_msn, pf_phpbb_yahoo, pf_phpbb_icq)")

    ctx.Users
    |> Seq.map (fun u -> u.Value)
    |> Seq.take 25
    |> Seq.iteri (fun i u ->
        let values = if i = 0 then "VALUES" else ""
        sql.WriteLine(sprintf "%-7s(%7d, %s, %s, %s, %s, %s, %s, %s, %s, %s)," values u.Id (sqlString u.CustomRank) (sqlStringOption u.Interests) (sqlStringOption u.Occupation) (sqlStringOption u.Location) (sqlStringOption u.XboxTag) (sqlStringOption u.Homepage) (sqlStringOption u.MSN) (sqlStringOption u.YM) (sqlStringOption u.ICQ))
    )

let internal writeForums (sql : IO.StreamWriter) (ctx : Context) =
    sql.WriteLine(@"
-- Inserting migrated forums.
INSERT INTO phpbb_forums
       (forum_id, forum_type, forum_flags, left_id, right_id, enable_icons, forum_name, forum_desc)")

    ctx.Forums
    |> Seq.map (fun f -> f.Value)
    |> Seq.iteri (fun i f ->
        let values = if i = 0 then "VALUES" else ""
        sql.WriteLine(sprintf "%-7s(%8d, %10d, %11d, %7d, %8d, %12d, %s, %s)," values f.Id 1 48 (f.Id) (f.Id + 1) 0 (sqlString f.Name) (sqlString f.Description))
    )

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
--ROLLBACK;
--COMMIT;
")