package calculation

class EuropeanReport {
  // € / t CO2
  val CO2Price2020 = 22; val CO2Price2030 = 24
  // USD / bbl
  val oilPrice2020 = 63; val oilPrice2030 = 63
  
  // Data from European Centre for Medium-Range Weather Forecasts (ECMWF)
 // Relationship between wind speed and full load hour
  def fullLoadHour(v : Double) = 626.64*v - 1977.5
}