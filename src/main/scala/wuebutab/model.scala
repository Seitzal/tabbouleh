package wuebutab

case class Tab(
val name: String = "new tab",
val teams: Map[Int, Team] = Map(),
val nextTeamId: Int = 1):

  def printSummary = Output(() => 
    println("Name: " + name)
    println("Teams: " + teams.size)
  )

  def printTeams = Output(() => 
    println(s"${teams.size} Teams:")
    println(
      teams.values.toSeq
        .sortBy(team => team.id)
        .map(team => s"${team.id}\t${team.name}")
        .mkString("\n"))
  )

object Tab:

  def addTeam(name: String): Action = context ?=>
    val team = Team(context.tab.nextTeamId, Map("Team" -> name))
    Result()
      .updateTab(Tab(
        context.tab.name,
        context.tab.teams + (context.tab.nextTeamId -> team),
        context.tab.nextTeamId + 1))
      .output(Output.Simple(context.tab.nextTeamId + "\t" + name))
