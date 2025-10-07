# Venezuela Economic Recovery Simulator 

Repository for a [Tableau Public](https://public.tableau.com/app/discover) and [RShiny](https://shiny.posit.co/) project with pedagogical purposes that simulates the estimated time to recovery for the Venezuelan economy, based on user input of base year and compound yearly growth rate. Scenario probability is approximated using the country's historical yearly growth rates from 1830 to 2024. Both standard and per capita versions of the simulator are available.

Basic instructions for the simulator:
1. Select a base year to fix a real GDP reference “target.”
2. Select a yearly compounding growth rate to calculate how long it takes for projected GDP to reach the target.
3. Restrict the reference period to see how (un)common the selected growth rate is compared to historical yearly GDP growth.

# Release V.1 (Spanish)

[Tableau Dashboard/Story](https://public.tableau.com/views/Escenariosderecuperacion/Story?:language=es-ES&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link)

For both standard and per capita (ppc) versions there are the following pages and modules:

1. Yearly real GDP growth rate (1830–2024) with historical context: See period of expansion and contraction for the Veneuzelan economy, with the 2013-2020 collapse noted as the largest and most severe. 
   
2. Dashboard with simulator:
   
   2.1  Controllers for base year, compounding growth rate, and period selector. Located at the bottom. The period selector only affects the histogram.
   
   2.2 Historic GDP index with projected GDP index from 2024 onwards: Displays a dynamic range based on the selected base year and growth rate. The projection extends until GDP surpasses the target.

   2.3 Histogram of yearly growth rates vs. selected growth rate: Shows the distribution of historical yearly growth rates, color-coded to highlight whether they exceed the selected growth rate. The year range is editable.
   
   2.4 Moving yearly compound growth rate comparison: Dynamically calculates the compound growth rate over periods equal in length to the estimated time to recovery. Compares these to the target rate to assess whether Venezuela has historically achieved similar or higher growth rates.
