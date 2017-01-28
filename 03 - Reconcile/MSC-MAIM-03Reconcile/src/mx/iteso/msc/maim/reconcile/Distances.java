package mx.iteso.msc.maim.reconcile;

import org.simmetrics.StringMetric;
import org.simmetrics.metrics.*;

public class Distances {
	public static void displayDistances(String a, String b) {
		StringMetric metric = new Levenshtein();
		System.out.println("Levenshtein: " + metric.compare(a, b));

		metric = StringMetrics.jaro();
		System.out.println("Jaro: " + metric.compare(a, b));

		metric = StringMetrics.dice();
		System.out.println("dice: " + metric.compare(a, b));

		metric = StringMetrics.euclideanDistance();
		System.out.println("euclidean: " + metric.compare(a, b));

		metric = StringMetrics.generalizedJaccard();
		System.out.println("Jaccard: " + metric.compare(a, b));

		metric = StringMetrics.longestCommonSubsequence();
		System.out.println("Longest common Subsequence: " + metric.compare(a, b));

		metric = StringMetrics.longestCommonSubstring();
		System.out.println("Longest common substring: " + metric.compare(a, b));

		metric = StringMetrics.mongeElkan();
		System.out.println("Monge Elkan: " + metric.compare(a, b));

		metric = StringMetrics.needlemanWunch();
		System.out.println("NeedlemanWunch: " + metric.compare(a, b));

		metric = StringMetrics.overlapCoefficient();
		System.out.println("Overlap Coefficient: " + metric.compare(a, b));

		metric = StringMetrics.qGramsDistance();
		System.out.println("Qgrams Distance: " + metric.compare(a, b));

		metric = StringMetrics.simonWhite();
		System.out.println("SimonWhite: " + metric.compare(a, b));

		metric = StringMetrics.smithWaterman();
		System.out.println("SmithWaterman: " + metric.compare(a, b));
	}
}
