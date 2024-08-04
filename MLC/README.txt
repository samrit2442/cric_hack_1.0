All Men's Major League Cricket match data in CSV format
=======================================================

The background
--------------

As an experiment, after being asked by a user of the site, I started
converting the YAML data provided on the site into a CSV format. That
initial version was heavily influenced by the format used by the baseball
project Retrosheet. I wasn't sure of the usefulness of my CSV format, but
nothing better was suggested so I persisted with it. Later Ashwin Raman
(https://twitter.com/AshwinRaman_) send me a detailed example of a format
he felt might work and, liking what I saw, I started to produce data in
a slightly modified version of that initial example.

This particular zip folder contains the CSV data for...
  All Men's Major League Cricket matches
...for which we have data.

How you can help
----------------

Providing feedback on the data would be the most helpful. Tell me what you
like and what you don't. Is there anything that is in the JSON data that
you'd like to be included in the CSV? Could something be included in a better
format? General views and comments help, as well as incredibly detailed
feedback. All information is of use to me at this stage. I can only improve
the data if people tell me what does works and what doesn't. I'd like to make
the data as useful as possible but I need your help to do it. Also, which of
the 2 CSV formats do you prefer, this one or the original? Ideally I'd like
to settle on a single CSV format so what should be kept from each?

Finally, any feedback as to the licence the data should be released under
would be greatly appreciated. Licensing is a strange little world and I'd
like to choose the "right" licence. My basic criteria may be that:

  * the data should be free,
  * corrections are encouraged/required to be reported to the project,
  * derivative works are allowed,
  * you can't just take data and sell it.

Feedback, pointers, comments, etc on licensing are welcome.

The format of the data
----------------------

Full documentation of this CSV format can be found at:
  https://cricsheet.org/format/csv_ashwin/
but the following is a brief summary of the details...

This format consists of 2 files per match, although you can get all of
the ball-by-ball data from just one of the files. The files for a match
are named <id>.csv (for the ball-by-ball data), and <id>_info.csv (for
the match info), where <id> is the Cricinfo id for the match. The
ball-by-ball file contains one row per delivery in the match, while the
match info file contains match information such as dates the match was
played, the outcome, and lists of the players involved in the match.

The match info file format
--------------------------

The info section contains the information on the actual match, such as
when and where it was played, any event it was part of, the type of
match etc. The fields included in the info section will each appear as
one or more rows in the data. Some of the fields are required, whereas
some are optional. If a field has multiple values, such as team, then
each value will appear on a row of it's own.

The ball-by-ball file format
----------------------------

The first row of each ball-by-ball CSV file contains the headers for the
file, with each subsequent row providing details on a single delivery.
The headers in the file are:

  * match_id
  * season
  * start_date
  * venue
  * innings
  * ball
  * batting_team
  * bowling_team
  * striker
  * non_striker
  * bowler
  * runs_off_bat
  * extras
  * wides
  * noballs
  * byes
  * legbyes
  * penalty
  * wicket_type
  * player_dismissed
  * other_wicket_type
  * other_player_dismissed

Most of the fields above should, hopefully, be self-explanatory, but some may
benefit from clarification...

"innings" contains the number of the innings within the match. If a match is
one that would normally have 2 innings, such as a T20 or ODI, then any innings
of more than 2 can be regarded as a super over.

"ball" is a combination of the over and delivery. For example, "0.3" represents
the 3rd ball of the 1st over.

"wides", "noballs", "byes", "legbyes", and "penalty" contain the total of each
particular type of extras, or are blank if not relevant to the delivery.

If a wicket occurred on a delivery then "wicket_type" will contain the method
of dismissal, while "player_dismissed" will indicate who was dismissed. There
is also the, admittedly remote, possibility that a second dismissal can be
recorded on the delivery (such as when a player retires on the same delivery
as another dismissal occurs). In this case "other_wicket_type" will record
the reason, while "other_player_dismissed" will show who was dismissed.

Matches included in this archive
--------------------------------

2024-07-07 - club - MLC - male - 1432729 - Los Angeles Knight Riders vs San Francisco Unicorns
2024-07-06 - club - MLC - male - 1432728 - MI New York vs Washington Freedom
2024-07-05 - club - MLC - male - 1432726 - Seattle Orcas vs MI New York
2024-07-05 - club - MLC - male - 1432727 - Los Angeles Knight Riders vs Texas Super Kings
2023-07-30 - club - MLC - male - 1381379 - Seattle Orcas vs MI New York
2023-07-28 - club - MLC - male - 1381378 - Texas Super Kings vs MI New York
2023-07-27 - club - MLC - male - 1381376 - MI New York vs Washington Freedom
2023-07-27 - club - MLC - male - 1381377 - Texas Super Kings vs Seattle Orcas
2023-07-25 - club - MLC - male - 1381375 - MI New York vs Seattle Orcas
2023-07-24 - club - MLC - male - 1381374 - San Francisco Unicorns vs Texas Super Kings
2023-07-23 - club - MLC - male - 1381372 - Seattle Orcas vs Los Angeles Knight Riders
2023-07-23 - club - MLC - male - 1381373 - Washington Freedom vs MI New York
2023-07-22 - club - MLC - male - 1381371 - Washington Freedom vs San Francisco Unicorns
2023-07-21 - club - MLC - male - 1381370 - Texas Super Kings vs Seattle Orcas
2023-07-20 - club - MLC - male - 1381369 - Los Angeles Knight Riders vs Washington Freedom
2023-07-18 - club - MLC - male - 1381368 - San Francisco Unicorns vs Los Angeles Knight Riders
2023-07-17 - club - MLC - male - 1381367 - Texas Super Kings vs MI New York
2023-07-16 - club - MLC - male - 1381365 - Washington Freedom vs Texas Super Kings
2023-07-16 - club - MLC - male - 1381366 - MI New York vs Los Angeles Knight Riders
2023-07-15 - club - MLC - male - 1381364 - Seattle Orcas vs San Francisco Unicorns
2023-07-14 - club - MLC - male - 1381362 - San Francisco Unicorns vs MI New York
2023-07-14 - club - MLC - male - 1381363 - Washington Freedom vs Seattle Orcas
2023-07-13 - club - MLC - male - 1381361 - Texas Super Kings vs Los Angeles Knight Riders

Consolidated data
-----------------

You may notice that there is an extra CSV file in this archive, called
"all_matches.csv". This file, as the name suggests, contains all of the
ball-by-ball data for matches from the archive in a single CSV. Hopefully
it will make use of the data easier in some cases.

Further information
-------------------

You can find all of our currently available data at https://cricsheet.org/

You can contact me via the following methods:
  Email  : stephen@cricsheet.org
  Twitter: @cricsheet
