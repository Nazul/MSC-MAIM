package mx.iteso.msc.maim.reconcile;

public class Accidents {
	private String nombreCalle, colonia, isoNombreCalle, accidentes;

	public Accidents(String nombreCalle, String colonia, String accidentes) {
		this.nombreCalle = nombreCalle;
		this.colonia = colonia;
		this.accidentes = accidentes;
		this.isoNombreCalle = "Pending";
	}

	String getNombreCalle() {
		return nombreCalle;
	}

	void setISONombreCalle(String isoNombreCalle) {
		this.isoNombreCalle = isoNombreCalle;
	}

	@Override
	public String toString() {
		return nombreCalle + "," + isoNombreCalle + "," + colonia + "," + accidentes;
	}
}

// EOF
