package wuebutab

@main def test: Unit = 
  // val teams = read_team_table_csv("teams_after5.csv")
  // val tab = Tab("Test Tab", teams)
  // val pairings = make_pairings(tab.active_teams, DebateType.Prepared, Weights.POWER_WINEXP)
  // println(render_table(pairings.sortBy(_.mean_rank_score).reverse))
  // val tab2 = tab.apply_pairings(pairings)
  // println(render_table(tab2.active_teams))
  // write_team_table_csv(tab2.teams, "teams_after8.csv")
  val teams = Seq(
    Team("Team 1"),
    Team("Team 2"),
    Team("Team 3"),
    Team("Team 4"),
    Team("Team 5"),
    Team("Team 6"))
  val judges = Seq(
    Judge("Judge 1", rating = 10, clashes = Vector("Team 1")),
    Judge("Judge 2", rating = 10, clashes = Vector("Team 2")),
    Judge("Judge 3", rating = 8, clashes = Vector("Team 3")),
    Judge("Judge 4", rating = 4),
    Judge("Judge 5", rating = 5),
    Judge("Judge 6", rating = 7, previously_seen = Vector("Team 1", "Team 3")),
    Judge("Judge 7", rating = 4, clashes = Vector("Team 4")),
    Judge("Judge 8", rating = 2),
    Judge("Judge 9", rating = 2, previously_seen = Vector("Team 1", "Team 2", "Team 3", "Team 4", "Team 5")))
  val pairings = make_pairings(teams, DebateType.Impromptu, Weights.POWER_WINEXP)
  make_panels(pairings, judges).foreach(p => println(s"${p.pairing.prop.name} vs. ${p.pairing.opp.name}: ${p.chair.name}(c), ${p.panelist1.map(_.name)}, ${p.panelist2.map(_.name)}"))
