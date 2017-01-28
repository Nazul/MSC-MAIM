package mx.iteso.msc.maim.reconcile;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import org.simmetrics.StringMetric;
import org.simmetrics.metrics.NeedlemanWunch;


public class Reconcile {
	public static void main(String[] args) {
		ArrayList<Accidents> accidents = new ArrayList<>();
		ArrayList<MasterDataStreets> mdStreets = new ArrayList<>();

		// 1. Read accidents data
		try {
			BufferedReader br = new BufferedReader(new FileReader(new File(
					"C:\\Users\\Mario_Contreras\\Dropbox\\MSC\\31. MAIM\\Code\\MSC-MAIM\\03 - Reconcile\\DataSet2.csv")));
			String line;
			// Header
			line = br.readLine();
			while ((line = br.readLine()) != null) {
				String[] entries = line.split(",", -1);

				Accidents accident = new Accidents(entries[0], entries[1], entries[2]);
				accidents.add(accident);
			}
			br.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("Accident data loaded");

		// 2. Read master data
		try {
			BufferedReader br = new BufferedReader(new FileReader(new File(
					"C:\\Users\\Mario_Contreras\\Dropbox\\MSC\\31. MAIM\\Code\\MSC-MAIM\\03 - Reconcile\\Catalog2.csv")));
			String line;
			// Header
			line = br.readLine();
			while ((line = br.readLine()) != null) {
				String[] entries = line.split(",", -1);

				MasterDataStreets mdStreet = new MasterDataStreets(entries[0], entries[1]);
				mdStreets.add(mdStreet);
			}
			br.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("Master data loaded");
		
		// Show all distances for all elements
		System.out.println("***** Show all distances for all elements *****");
		for (Accidents accident : accidents) {
			for (MasterDataStreets mdStreet : mdStreets) {
				System.out.println("--- Compare " + accident.getNombreCalle() + " vs " + mdStreet.getIsoNombreCalle() + "---");
				Distances.displayDistances(accident.getNombreCalle(), mdStreet.getIsoNombreCalle());
			}
		}
		
		System.out.println();
		System.out.println("***** Reconcile using Needleman Wunch *****");
		// Compare all
		StringMetric metric = new NeedlemanWunch();

		for (Accidents accident : accidents) {
			float higherScore = -1000;
			String isoStreet = "";
			for (MasterDataStreets mdStreet : mdStreets) {
				float distance = metric.compare(accident.getNombreCalle(), mdStreet.getIsoNombreCalle());
				System.out.println("Compare " + accident.getNombreCalle() + " vs " + mdStreet.getIsoNombreCalle() + ". Similarity: " + distance);
				if (distance > higherScore) {
					higherScore = distance;
					isoStreet = mdStreet.getIsoNombreCalle();
				}
			}
			accident.setISONombreCalle(isoStreet);
		}
		for (Accidents accident : accidents) {
			System.out.println(accident.toString());
		}
	}
}

// EOF
