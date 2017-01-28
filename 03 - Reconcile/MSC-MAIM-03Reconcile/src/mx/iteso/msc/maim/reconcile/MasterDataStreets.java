package mx.iteso.msc.maim.reconcile;

public class MasterDataStreets {
	private String id, isoNombreCalle;

	public MasterDataStreets(String id, String isoNombreCalle) {
		this.id = id;
		this.isoNombreCalle = isoNombreCalle;
	}

	public String getIsoNombreCalle() {
		return isoNombreCalle;
	}
}

// EOF
