package embodiedEnergy

import squants.energy.Energy
import squants.energy.Gigajoules

case class Material(val energyIntensity : Energy, val unit : String)

class Concrete20MPa extends Material(Gigajoules(4.08), "CubicMeters")
class Steel extends Material(Gigajoules(85.3), "Tonnes")
class Aluminium extends Material(Gigajoules(252),"Tonnes")
class Copper extends Material(Gigajoules(379),"Tonnes")
class GlassFibre extends Material(Gigajoules(168), "Tonnes")
class Epoxy extends Material(Gigajoules(163), "Tonnes")
class Paint extends Material(Gigajoules(0.096), "SquareMetres")

