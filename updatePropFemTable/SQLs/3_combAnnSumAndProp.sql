SELECT
QUERY_CPUE_ANNUAL_SUMMARY.Year,
QUERY_CPUE_ANNUAL_SUMMARY.Pot_Count,
 Round([Total_Spot_Wt_KG]*2.20462) AS TotalLbs,
 Round(QUERY_CPUE_ANNUAL_SUMMARY.CPUE_All_LB,2) AS PndsPerPot,
 QUERY_CPUE_ANNUAL_SUMMARY.Total_Spot_Count AS TotSpots,
 Round(([propMales]*100),1) AS perMales,
 Round(([propFems]*100),1) AS perFems,
 Round(([propEgg]*100),1) AS perEgg
 
 FROM QUERY_CPUE_ANNUAL_SUMMARY 
 LEFT JOIN temp ON QUERY_CPUE_ANNUAL_SUMMARY.Year = temp.YEAR;

