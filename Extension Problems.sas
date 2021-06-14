proc import out=demand
	datafile = "/home/u47247130/sasuser.v94/optimisation/demand_num.csv"
	replace;
run; 

proc import out=tariffs
	datafile = "/home/u47247130/sasuser.v94/optimisation/tariffs.csv"
	replace;
run; 

/* to simplify the codes, regard WTP as a Tank */
data supply;
   infile datalines;
   input  ORIGIN $ MinVolume MaxVolume initial ;
   datalines;
Clear   100   500  300
Tank_E  200   375  350
Tank_P  100   300  250
Tank_N  300	  1000  350
WTP     0     7200	7200
;

data nodes;
   infile datalines;
   input  ORIGIN $  DESTINATION $;
   datalines;
WTP      Clear   
Clear    Tank_E  
Clear    Tank_P    
Tank_E   Ellersli  
Tank_E   Tank_N
Tank_N   Newmarke
Tank_P   Penrose
Tank_P   Tank_N
;

data hour;
   do h = 1 to 24;
      output;
   end;
run;


proc optmodel ;
	/* declare sets and parameters */
	set <str,str> ARCS;
	set <str> ORIGINS, DESTINATIONS;
	set <num> HOURS;
	num demand {DESTINATIONS, HOURS};
	num MinVolume {ORIGINS} init 0;
	num MaxVolume {ORIGINS} init 0;
	num initial   {ORIGINS} init 0;
	num tariff1   {HOURS};
	num tariff2   {HOURS};
	
	/* the constant coefficient to calculate energy */
	num a = 1000*9.8/(0.65*3.6*10^6);
	print a;
	
	/* read data from SAS data sets */
	read data hour into HOURS=[h];
	read data nodes into ARCS=[Origin Destination] ;
	read data demand into DESTINATIONS=[Destination] {h in HOURS} <demand[destination,h]=col('h'||h)> ; 
	read data supply into ORIGINS=[Origin] MinVolume MaxVolume initial;
	read data tariffs into [Hour] tariff1 tariff2 ;
	print demand;
	print MinVolume MaxVolume initial;
	
	/* Classify assistant tags*/
	set NODES = union {<i,j> in ARCS} {i,j};

	put ORIGINS DESTINATIONS;
	
	/* declare variables */
	var NumFlow {ARCS, HOURS} >= 0;
	var Volume {k in ORIGINS, h in HOURS } >= 0;
	var y1{h in HOURS} binary;
	var y2{h in HOURS} binary;
	impvar FlowIn {k in NODES, h in HOURS} = sum {<i,(k)> in ARCS} NumFlow[i,k,h];
	impvar FlowOut {k in NODES,h in HOURS} = sum {<(k),j> in ARCS} NumFlow[k,j,h];
	impvar cost1 {h in HOURS} = NumFlow["Clear","Tank_E",h] * a * 90 * tariff1[h];
	impvar cost2 {h in HOURS} = NumFlow["Tank_P","Tank_N",h] * a * 100 * tariff2[h];
	
	/* declare constraints*/		
	con supply_con {k in ORIGINS,h in HOURS}:
		FlowIn[k,h] + Volume[k,h] >= FlowOut[k,h];
		
	con demand_con {k in DESTINATIONS, h in HOURS}:
		FlowIn[k,h] = demand[k,h];
	
	con supply_WTP {h in HOURS}:
		90 <= NumFlow["WTP","Clear",h] <= 300 ;
		
	/* extension 1 */			
	set h_even = (setof{h in HOURS: h<13} h);	
	con extension_WTP {h in h_even}:
		NumFlow["WTP","Clear",2*h-1]=NumFlow["WTP","Clear",2*h] ;
	
	/* Volume constraints for Tanks*/
	/* assistant numbers for Volume constraints */
	num final = max {h in HOURS} h;
	num primary = min {h in HOURS} h;
	set MIDDLE = (setof{h in HOURS: h>1} h);
	
	con Tank_supply {k in ORIGINS, h in HOURS}: 
		MinVolume[k] <=  Volume[k,h] <= MaxVolume[k];
	con final_Volume {k in ORIGINS}:
		Volume[k,final]= initial[k];
	con primary_Volume {k in ORIGINS}:
		Volume[k,primary] = initial[k];
	con middle_Volume {k in ORIGINS, h in MIDDLE}:
		Volume[k,h] = Volume[k,h-1] - FlowOut[k,h-1] + FlowIn[k,h-1];
	
	/* Valve constraints*/
	con binary_con1:
		sum{h in HOURS} y1[h] <= 24;
	con binary_con2:
		sum{h in HOURS} y2[h] <= 24;
	con Valve1_con1 {h in HOURS}:
		0 <= NumFlow["Tank_E", "Tank_N", h]- 20*y1[h] ;
	con Valve1_con2 {h in HOURS}:
		NumFlow["Tank_E", "Tank_N", h] <= 100;
	con Valve1_con3 {h in HOURS}:
		NumFlow["Tank_E", "Tank_N", h] - 10000*y1[h] <= 0 ;		
	con Valve2_con1 {h in HOURS}:
		0 <= NumFlow["Clear", "Tank_P", h]- 50*y2[h] ;
	con Valve2_con2 {h in HOURS}:
		NumFlow["Clear", "Tank_P", h] <= 180;
	con Valve2_con3 {h in HOURS}:
		NumFlow["Clear", "Tank_P", h] - 10000*y2[h] <= 0 ;	
	
	/* declare objective */
	min TotalCost = sum{h in HOURS} cost1[h] + sum{h in HOURS} cost2[h];
	
	solve;
	print {<i,j> in ARCS, h in HOURS} NumFlow[i,j,h];
	print {k in ORIGINS, h in HOURS} Volume[k,h];	
	print {h in HOURS} cost1;
	print {h in HOURS} cost2;
quit;
		
		
		
		
		
		
		
		
		
	