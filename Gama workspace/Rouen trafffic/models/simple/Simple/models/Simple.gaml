/**
* Name: Simple
* Based on the internal empty template. 
* Author: chris
* Tags: 
*/


model Simple

/* Insert your model definition here */


global {   
	file shape_file_roads  <- file("../includes/roads.shp") ;
	file shape_file_nodes  <- file("../includes/nodes.shp");
	geometry shape <- envelope(shape_file_roads);
	
	graph road_network;
	int nb_people <- 2;
	int n_modified_roads <- 1;
	int n_accidents <- 1;
	float car_length <- 3 #m;
	float weight <- 100000.0;
	float waze_percentage <- 1.0;
	int nb_waze <- int(nb_people*waze_percentage);
	map general_speed_map;
	
	float prob_car0 <- 1/nb_people
	update:max(people count (each.speed <10 #km/#h) / nb_people, 1/nb_people);
	
	float prob_car1 <- 1/nb_people
	update:max(people count (each.speed >=10 #km/#h and each.speed<20 #km/#h) 
		/ nb_people, 1/nb_people);
	
	float prob_car2 <- 1/nb_people
	update:max(people count (each.speed >=20 #km/#h and each.speed<30 #km/#h) 
		/ nb_people, 1/nb_people);
	
	float prob_car3 <- 1/nb_people
	update:max(people count (each.speed >=30 #km/#h and each.speed<40 #km/#h) 
		/ nb_people, 1/nb_people);
	
	float prob_car4 <- 1/nb_people
	update:max(people count (each.speed >=40 #km/#h) / nb_people, 1/nb_people);
	
	float entropie <- 0.0 update: -(prob_car0*ln(prob_car0)+prob_car1*ln(prob_car1)+
		prob_car2*ln(prob_car2)+prob_car3*ln(prob_car3)+prob_car4*ln(prob_car4));
		
	float gini_index <- 0.0 update: gini([prob_car0, prob_car1, prob_car2, prob_car3, prob_car4]);
	
	int nb_hasAcc <- 0 update:road count(each.hasAcc = 1);
	
	int nb_stoppedCars_W <- people count(each.real_speed <= 0.1 and each.hasWaze) update: people count(each.real_speed <= 0.1 and each.hasWaze);
	int nb_stoppedCars_R <- people count(each.real_speed <= 0.1 and not(each.hasWaze)) update: people count(each.real_speed <= 0.1 and not(each.hasWaze));
	
	init {  
		create roadNode from: shape_file_nodes 
		with:[is_traffic_signal::(string(read("type")) = "traffic_signals"),
			is_A::(string(read("type")) = "A"),
			is_B::(string(read("type")) = "B")
		];
		ask roadNode where each.is_traffic_signal {
			stop << flip(0.5) ? roads_in : [] ;
		}
		create road from: shape_file_roads with:[lanes::int(read("lanes")), 
			maxspeed::float(read("maxspeed")) °km/°h, oneway::string(read("oneway")),
			is_mid::(string(read("type"))="mid"),is_top::(string(read("type"))="top")
		] {
			geom_display <- (shape + (2.5 * lanes));
			switch oneway {
				match "no" {
					create road {
						lanes <- myself.lanes;
						shape <- polyline(reverse(myself.shape.points));
						maxspeed <- myself.maxspeed;
						geom_display  <- myself.geom_display;
						linked_road <- myself;	
						myself.linked_road <- self;
						hasAcc <- 0;
						weightAdded <- 0;
					}
				}
				match "-1" {
					shape <- polyline(reverse(shape.points));
				}
			}
		}	
		general_speed_map <- road as_map (each::(each.shape.perimeter / (each.maxspeed)));
		road_network <-  (as_driving_graph(road, roadNode))  with_weights general_speed_map;
		write "number of roads : "+length(road);
		write "number of noad roads : "+length(roadNode);
		create people number: nb_people { 
			speed <- 30 #km /#h ;
			vehicle_length <- car_length;
			right_side_driving <- true;
			proba_lane_change_up <- rnd(1.0);
			proba_lane_change_down <- rnd(0.5,1.0);
			//location <- one_of(nodeRoad).location;
			security_distance_coeff <- rnd(1.0,3.0);
			proba_respect_priorities <- rnd(0.8,1.0);
			proba_respect_stops <- [rnd(0.998,1.0)];
			proba_block_node <- rnd(0.0,0.003);
			proba_use_linked_road <- 0.0;
			max_acceleration <- rnd(0.5,1.0);
			speed_coeff <- rnd(0.8,1.2);
			if flip(0.5) {
				location <- one_of(roadNode where (each.is_A)).location;
				target <- one_of(roadNode where (each.is_B));
			}
			else{
				location <- one_of(roadNode where (each.is_B)).location;
				target <- one_of(roadNode where (each.is_A));
			}

		}	
		ask nb_waze among people{
				hasWaze <- true;
				color <- rgb("blue");
		} 	
	}
		
} 
species roadNode skills: [skill_road_node] {
	bool is_traffic_signal;
	bool is_A<-false;
	bool is_B<-false;
	int time_to_change <- 100;
	int counter <- rnd (time_to_change) ;
	
	reflex dynamic when: is_traffic_signal {
		counter <- counter + 1;
		if (counter >= time_to_change) { 
			counter <- 0;
			stop[0] <- empty (stop[0]) ? roads_in : [] ;
		} 
	}
	
	aspect geom3D {
		if (is_traffic_signal) {	
			draw box(1,1,10) color:rgb("black");
			draw sphere(5) at: {location.x,location.y,12} color: empty (stop[0]) ? #green : #red;
		}
	}
}

species road skills: [skill_road] { 
	bool is_top <- false;
	bool is_mid <- false;
	int hasAcc;
	int weightAdded;
	string oneway;
	geometry geom_display;
	float car_coeff update: (people at_distance 6#m count(each.speed < 0.5*self.maxspeed))/20;
	//personnes aux alentours de la route qui ont une speed < 0.5*maxspeed de la route / 20
	int colorValue <- int(255*(car_coeff)) update: int(255*(car_coeff));
    rgb color <- rgb(min([255, colorValue]),max ([0, 255 - colorValue]),0)
    	update: rgb(min([255, colorValue]),max ([0, 255 - colorValue]),0);
	aspect geom {
		draw geom_display border: #gray  color: color;
	}
	
	action add_weight{
		put weight key:self in: general_speed_map;
		weightAdded <- 1;
	}
	
	action add_accident{
		hasAcc <- 1;
		maxspeed <- 0.0;
	}
	
	user_command cmd_add_weight action:add_weight;
}
	
species people skills: [advanced_driving] { 
	bool hasWaze ;
	rgb color <- rgb("orange");
	roadNode target;
	
	reflex time_to_go when: final_target = nil {
		if target.is_A{
			target <- one_of(roadNode where (each.is_B));
		}
		else{
			target <- one_of(roadNode where (each.is_A));
		}
		//target <- one_of(roadNode);
		current_path <- compute_path(graph: road_network, target: target);
	}
	reflex move when: final_target != nil {
		do drive;
	}
	
	reflex closenode when: string(type_of(agent_closest_to(self))) = "roadNode"{
		write "yes" ;
		
		ask road where (each.hasAcc=1 and each.weightAdded=0){
			do add_weight;
		}
		road_network <- road_network with_weights general_speed_map;
		
		
		if (current_path.vertices[current_index+1]!=target) {
			list<path> var0 <- paths_between(road_network, current_path.vertices[current_index+1]::target, 2);
			if (length(var0) =2 ){
        	current_path <- var0[rnd(0,1)];
    		}
		}
	}
	
	reflex is_at_node when: location  = current_target{
		 
		
		ask road where (each.hasAcc=1 and each.weightAdded=0){
			do add_weight;
		}
		road_network <- road_network with_weights general_speed_map;
		
		if (current_path.vertices[current_index+1]!=target) {
			list<path> var0 <- paths_between(road_network, current_path.vertices[current_index+1]::target, 2);
			if (length(var0) =2 ){
        	current_path <- var0[rnd(0,1)];
    		}
		}
    }
	
	aspect car3D {
		if (current_road) != nil {
			point loc <- calcul_loc();
			draw box(vehicle_length, 1,1) at: loc rotate:  heading color: color;
			draw triangle(0.5) depth: 1.5 at: loc rotate:  heading + 90 color: color;	
		}
	} 
	
	point calcul_loc {
		float val <- (road(current_road).lanes - current_lane) + 0.5;
		val <- on_linked_road ? val * - 1 : val;
		if (val = 0) {
			return location; 
		} else {
			return (location + {cos(heading + 90) * val, sin(heading + 90) * val,1});
		}
	}	
	
	action accident{
		color <- rgb("black");
		speed_coeff <- 0.0;
		road(current_road).hasAcc <- 1; 
	}
	
} 

experiment traffic_simulation type: gui {
	//parameter "Nb road to modify" var: n_modified_roads;
	parameter "weight to add in the graph" var: weight;
	parameter "Nb accidents to add" var: n_accidents;
	
	action add_n_road_accidents{
		ask n_modified_roads among road {
			do add_accident;
		}
		write string(n_modified_roads)+" roads modified!";
		write "Time: "+string(time);
	}
	
	action add_n_accidents{
		ask n_accidents among people{
			do accident;
		}
	}
	
	action add_mid_acc{
		ask 1 among (people where(road(each.current_road).is_mid)){
			do accident;
		}
	}
	
	action add_top_acc{
		ask 1 among (people where(road(each.current_road).is_top)){
			do accident;
		}
	}
	
	action recompute_path{
		ask road where (each.hasAcc=1 and each.weightAdded=0){
			do add_weight;
		}
		road_network <- road_network with_weights general_speed_map;
		ask people where (each.hasWaze){
			
			if (current_path.vertices[current_index] != target){
				list<path> var0 <- paths_between(road_network, current_path.vertices[current_index+1]::target, 2); // var0 equals the 2 shortest paths (ordered by length) between ag1 and ag2
				write var0;
				//write var0;
				//write current_road;
				//write current_target;
				//write current_path.vertices[current_index+1];
				//write target;
				if (length(var0) != 0 ){
					if(location  = current_target){
						current_path <- var0[rnd(0,length(var0)-1)];
						write current_path;
					}
					
				}
			}
		}
	}
	
	//user_command cmd_add_n_road_accidents action: add_n_road_accidents;
	user_command cmd_recompute_path action:recompute_path;
	user_command cmd_add_n_accidents action:add_n_accidents;
	user_command cmd_add_mid_acc action:add_mid_acc;
	user_command cmd_add_top_acc action:add_top_acc;
	
	output {
		display city_display type: opengl{
			species road aspect: geom;
			species roadNode aspect: geom3D;
			species people aspect: car3D;
		}
		
		display histogramme_vitesse {
			chart "histogrammes des vitesses des voitures" type: series{
				data "vitesse < 10km/h" value: prob_car0 color: #red;
				data "vitesse >= 10km/h et <20 km/h" value: prob_car1 color: #orange;
				data "vitesse >= 20km/h et <30 km/h" value: prob_car2 color: #yellow;
				data "vitesse >= 30km/h et <40 km/h" value: prob_car3 color: rgb(124,252,0);
				data "vitesse >= 40km/h" value: prob_car4 color: #green;
			}
		}
		display entropie{
			chart "entropie des vitesses du réseau" type: series{
				data "entropie" value: entropie color: #red;
			}
		}
		display gini{
			chart "gini index" type: series{
				data "gini index" value: gini_index color: #red;
			}
		}
		display nb_stoppedCars{
			chart "Nombre de voitures arrêtées" type: series{
				data "voitures avec Waze" value: nb_stoppedCars_W color: #blue;
				data "voitures sans Waze" value: nb_stoppedCars_R color: #orange;
			}
		}
	}
}