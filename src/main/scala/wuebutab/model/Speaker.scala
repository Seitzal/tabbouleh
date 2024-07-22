package wuebutab

case class Speaker(
  homeTeam: String,
  harmonizedName: String,
  speeches: Vector[Speech]):

  def speechesRegular = speeches.filter(speech => speech.team == homeTeam && speech.speechType == "Main")
  def repliesRegular = speeches.filter(speech => speech.team == homeTeam && speech.speechType == "Reply")
  def speechesSwingMixed = speeches.filter(speech => speech.team != homeTeam && speech.speechType == "Main")
  def repliesSwingMixed = speeches.filter(speech => speech.team != homeTeam && speech.speechType == "Reply")

  def roundsSpoken = speeches.map(_.round).distinct

  def ballotsRegular = speechesRegular.length + repliesRegular.length * 0.5
  def ballotsSwingMixed = speechesSwingMixed.length + repliesSwingMixed.length * 0.5
  
  def pointsRegular = speechesRegular.map(_.score).sum + repliesRegular.map(_.score).sum
  def pointsSwingMixed = speechesSwingMixed.map(_.score).sum + repliesSwingMixed.map(_.score).sum

  def averageRegular = pointsRegular / ballotsRegular
  def averageSwingMixed = pointsSwingMixed / ballotsSwingMixed

  def points = 
    if ballotsRegular == 0 || averageSwingMixed > averageRegular 
    then pointsSwingMixed + pointsRegular
    else pointsRegular

  def ballots = 
    if ballotsRegular == 0 || averageSwingMixed > averageRegular 
    then ballotsSwingMixed + ballotsRegular
    else ballotsRegular

  def average = points / ballots

object Speaker:

  def getAll(names: Names, speeches: Vector[Speech]): Vector[Speaker] =
    val unique_names = 
      names.harmonized.toVector
      .map((name, harmonized) => Name(name.team, harmonized))
      .distinct
      .filter(name => name.team != "" && name.name != "NA")
    val speeches_grouped = unique_names.map(name => 
      speeches.filter(speech => 
        speech.homeTeam == name.team && speech.harmonizedName == name.name))
    for (name, speeches) <- unique_names zip speeches_grouped 
      if !speeches.isEmpty
      yield Speaker(name.team, name.name, speeches)

  given t: Tabulatable[Speaker] = new Tabulatable:

    def fields = Seq(
      TableField("Team", _.homeTeam, false),
      TableField("Name", _.harmonizedName, false),
      TableField("Rounds", _.roundsSpoken.length.toString, true),
      TableField("Points", _.points.toString, true),
      TableField("Ballots", _.ballots.toString, true),
      TableField("Average", _.average.dpl(4), true)
    )
