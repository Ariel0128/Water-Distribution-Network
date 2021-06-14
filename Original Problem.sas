data supply;
   infile datalines;
   input  ORIGIN $ MinVolume MaxVolume initial ;
   datalines;
Clear 100 500 300
Tank_E 200 375 350
Tank_P 100 300 250
Tank_N 300 1000 350
;

data tariffs;
	infile datalines;
	input Hour Tariff1 Tariff2;
	datalines;
1 0.1 0.06
2 0.1 0.06
3 0.1 0.06
4 0.17 0.06
5 0.17 0.12
6 0.17 0.12
7 0.24 0.12
8 0.24 0.12
9 0.24 0.18
10 0.24 0.18
11 0.24 0.18
12 0.24 0.18
13 0.17 0.12
14 0.17 0.12
15 0.17 0.12
16 0.17 0.06
17 0.17 0.06
18 0.17 0.06
19 0.1 0.06
20 0.1 0.06
21 0.1 0.06
22 0.1 0.06
23 0.1 0.06
24 0.1 0.06
;

data demand;
	infile datalines;
	input Destination $ h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 h14 h15 h16 h17 h18 h19 h20 h21 h22 h23 h24;
	datalines;
Ellersli 30 60 60 40 30 20 25 20 20 25 25 30 40 50 50 50 30 20 20 20 20 20 20 20
Penrose 15 45 45 25 15 5 10 5 5 10 10 15 25 35 35 35 15 5 5 5 5 5 5 5
Newmarke 50 100 100 200 200 250 300 400 350 300 300 400 300 200 100 50 50 50 50 50 50 50 50 50
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
proc optmodel;
	/* declare sets and parameters */
	set <str,str> ARCS;
	set <num> HOURS;
	set NODES = union {<i,j> in ARCS} {i,j};
	num demand {NODES, HOURS} init 0;
	num MinVolume {NODES} init 0;
	num MaxVolume {NODES} init 0;
	num initial {NODES} init 0;
	num tariff1 {HOURS};
	num tariff2 {HOURS};
	
	/* the constant coefficient to calculate energy */
	num a =1000*9.8/(0.65 * 3.6 *10^6);
	
	/* read data from SAS data sets */
	read data hour into HOURS=[h];
	read data nodes into ARCS=[Origin Destination];
	read data demand into [Destination] {h in HOURS} <demand[destination,h]=col('h'||h)>;
	read data supply into [Origin] MinVolume MaxVolume initial;
	read data tariffs into [Hour] tariff1 tariff2;
	
	/* Classify assistant tags*/
	set ORIGINS = (setof{j in NODES, h in HOURS: initial[j]=0 & demand[j,h]=0} j);
	set DESTINATIONS =  (setof{j in NODES, h in HOURS: initial[j]=0 & demand[j,h]>0} j);
	set TRANSSHIPMENT =   (setof{j in NODES,h in HOURS: initial[j]>0 & demand[j,h]=0} j);
	put ORIGINS;
	put ({TRANSSHIPMENT} union {DESTINATIONS});
	
	/* declare variables */
	var NumFlow {ARCS, HOURS} >=0;
	var Volume {k in TRANSSHIPMENT, HOURS union {0}} >=0;
	var y1 {h in HOURS} binary;
	var y2 {h in HOURS} binary;
	impvar FlowIn {k in NODES, h in HOURS} = sum{<i,(k)> in ARCS} NumFlow[i,k,h];
	impvar FlowOut {k in NODES, h in HOURS} = sum{<(k),j> in ARCS} NumFlow[k,j,h];
	impvar cost1 {h in HOURS} = NumFlow["Clear","Tank_E",h] * a * 90 * tariff1[h];
	impvar cost2 {h in HOURS} = NumFlow["Tank_P","Tank_N",h] * a * 100 * tariff2[h];
	/* declare constraints*/		
	con supply_con {k in TRANSSHIPMENT, h in HOURS}:
		FlowIn[k,h] + Volume[k,h] >= FlowOut[k,h];
	con demand_con {k in DESTINATIONS, h in HOURS}:
		FlowIn[k,h] = demand[k,h];
	
	/* NumFlow constraints for WTP*/
	con supply_WTP {h in HOURS}:
		90 <= NumFlow["WTP","Clear",h] <= 300;
	
	/* NumFlow constraints for pumps*/
	con pump1 {h in HOURS}:
		NumFlow["Clear","Tank_E",h] <= 180;
	con pump2 {h in HOURS}:
		NumFlow["Tank_P","Tank_N",h] <= 200;

	/* Volume constraints for Tanks*/
	/* assistant numbers for Volume constraints */
	num final = max {h in HOURS} h;
	
	con Tank_supply {k in TRANSSHIPMENT, h in HOURS}:
		MinVolume[k] <= Volume[k,h] <= MaxVolume[k];
	con final_Volume {k in TRANSSHIPMENT}:
		Volume[k,final]=initial[k];
	con primary_Volume {k in TRANSSHIPMENT}:
		Volume[k,0] = initial[k];
	con middle_Volume {k in TRANSSHIPMENT, h in HOURS}:
		Volume[k,h] = Volume[k,h-1] - FlowOut[k,h] + FlowIn[k,h];
	
	/* Valve constraints*/	
	con binary_con1:
		sum{h in HOURS} y1[h] <= 24;
	con binary_con2:
		sum{h in HOURS} y2[h] <= 24;
	con Valve1_con1 {h in HOURS}:
		0 <= NumFlow["Tank_E","Tank_N",h] - 20*y1[h];
	con Valve1_con2 {h in HOURS}:
		NumFlow["Tank_E","Tank_N",h] <= 100;
	con Valve1_con3 {h in HOURS}:
		NumFlow["Tank_E","Tank_N",h] - 10000*y1[h] <= 0;
	con Valve2_con1 {h in HOURS}:
		0 <= NumFlow["Clear","Tank_P",h] - 50*y2[h];
	con Valve2_con2 {h in HOURS}:
		NumFlow["Clear","Tank_P",h] <= 180;
	con Valve2_con3 {h in HOURS}:
		NumFlow["Clear","Tank_P",h] - 10000*y2[h] <= 0;
		
	/* declare objective */
	min TotalCost = sum{h in HOURS} cost1[h] + sum{h in HOURS} cost2[h];
	solve;
	
	print {<i,j> in ARCS, h in HOURS} NumFlow[i,j,h];
	print {k in TRANSSHIPMENT, h in HOURS} Volume[k,h];
	print {h in HOURS} cost1;
	print {h in HOURS} cost2;
quit;
		