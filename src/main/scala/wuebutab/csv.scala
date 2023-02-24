package wuebutab

import com.github.tototoshi.csv._

def read_team_table_csv(path: String): Vector[Team] =
  val reader = CSVReader.open(java.io.File(path))
  val teams = reader.iteratorWithHeaders.map(Team.apply).toVector
  reader.close()
  teams

def write_team_table_csv(teams: Seq[Team], path: String): Unit =
  val writer = CSVWriter.open(java.io.File(path))
  writer.writeAll(
    Seq(
      Seq("Team", "Active", "Division", "Wins", "Ballots", "Points", "Rank Score", "Balance", "Prep", "Impr", "Pull-ups") 
      ++ (1 to teams.head.previous_opponents.length).map("Opponent " + _.toString)
    ) ++ teams.map(team => 
      Seq(
        team.name,
        team.active,
        team.division,
        team.wins,
        team.ballots,
        team.points,
        team.rank_score,
        team.side_pref.overall,
        team.side_pref.prep,
        team.side_pref.impr,
        team.pull_ups
      ) ++ team.previous_opponents
    )
  )
