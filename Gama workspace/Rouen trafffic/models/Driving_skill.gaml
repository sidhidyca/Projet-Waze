/**
* Name: Complex Road Network 
* Author: Patrick Taillandier
* Description: Model to show how to use the driving skill to represent the traffic on a road network imported from shapefiles (generated, for the city, with 
* OSM Loading Driving), with intersections and traffic lights going from red to green to let people move or stop. Two experiments are presented : one concerning a 
* a simple ring network and the other a real city network.
* Tags: gis, shapefile, graph, agent_movement, skill, transport
*/
model RoadTrafficComplex

global {
	bool  display3D<- false;
	
	//Check if we use simple data or more complex roads
	file shape_file_roads <-  file("../includes/roads.shp");
	file shape_file_nodes <- file("../includes/nodes.shp");
	geometry shape <- envelope(shape_file_roads) + 50.0;
	graph road_network;
	
	int nb_people <- 500;
	int n_accidents <- 0;
	
	float waze_percentage <- 0.5;
	int nb_waze <- int(nb_people*waze_percentage);
	int nb_nonwaze <- nb_people - nb_waze;
	
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
	//create the intersection and check if there are traffic lights or not by looking the values inside the type column of the shapefile and linking
	// this column to the attribute is_traffic_signal. 
		create intersection from: shape_file_nodes with: [is_traffic_signal::(read("type") = "traffic_signals")];

		//create road agents using the shapefile and using the oneway column to check the orientation of the roads if there are directed
		create road from: shape_file_roads with: [lanes::int(read("lanes")), oneway::string(read("oneway"))] {
			geom_display <- shape + (2.5 * lanes);
			maxspeed <- (lanes = 1 ? 30.0 : (lanes = 2 ? 50.0 : 70.0)) °km / °h;
			switch oneway {
				match "no" {
					create road {
						lanes <- max([1, int(myself.lanes / 2.0)]);
						shape <- polyline(reverse(myself.shape.points));
						maxspeed <- myself.maxspeed;
						geom_display <- myself.geom_display;
						linked_road <- myself;
						myself.linked_road <- self;
						hasAcc <- 0;
					}

					lanes <- int(lanes / 2.0 + 0.5);
				}

				match "-1" {
					shape <- polyline(reverse(shape.points));
				}

			}

		}

		map general_speed_map <- road as_map (each::(each.shape.perimeter / each.maxspeed));

		//creation of the road network using the road and intersection agents
		road_network <- (as_driving_graph(road, intersection)) with_weights general_speed_map;

		//initialize the traffic light
		ask intersection {
			do initialize;
		}

		create people number: nb_people {
			max_speed <- 160 #km / #h;
			vehicle_length <- 5.0 #m;
			right_side_driving <- true;
			proba_lane_change_up <- 0.1 + (rnd(500) / 500);
			proba_lane_change_down <- 0.5 + (rnd(500) / 500);
			location <- one_of(intersection where empty(each.stop)).location;
			security_distance_coeff <- 5 / 9 * 3.6 * (1.5 - rnd(1000) / 1000);
			proba_respect_priorities <- 1.0 - rnd(200 / 1000);
			proba_respect_stops <- [1.0];
			proba_block_node <- 0.0;
			proba_use_linked_road <- 0.0;
			max_acceleration <- 5 / 3.6;
			speed_coeff <- 1.2 - (rnd(400) / 1000);
			threshold_stucked <- int((1 + rnd(5)) #mn);
		}
		
		ask nb_waze among people{
				hasWaze <- true;
				color <- rgb("blue");
		}

	}

}

//species that will represent the intersection node, it can be traffic lights or not, using the skill_road_node skill
species intersection skills: [skill_road_node] {
	bool is_traffic_signal;
	list<list> stop;
	int time_to_change <- 100;
	int counter <- rnd(time_to_change);
	list<road> ways1;
	list<road> ways2;
	bool is_green;
	rgb color_fire;

	action initialize {
		if (is_traffic_signal) {
			do compute_crossing;
			stop << [];
			if (flip(0.5)) {
				do to_green;
			} else {
				do to_red;
			}

		}

	}

	action compute_crossing {
		if (length(roads_in) >= 2) {
			road rd0 <- road(roads_in[0]);
			list<point> pts <- rd0.shape.points;
			float ref_angle <- float(last(pts) direction_to rd0.location);
			loop rd over: roads_in {
				list<point> pts2 <- road(rd).shape.points;
				float angle_dest <- float(last(pts2) direction_to rd.location);
				float ang <- abs(angle_dest - ref_angle);
				if (ang > 45 and ang < 135) or (ang > 225 and ang < 315) {
					ways2 << road(rd);
				}

			}

		}

		loop rd over: roads_in {
			if not (rd in ways2) {
				ways1 << road(rd);
			}

		}

	}

	action to_green {
		stop[0] <- ways2;
		color_fire <- #green;
		is_green <- true;
	}

	action to_red {
		stop[0] <- ways1;
		color_fire <- #red;
		is_green <- false;
	}

	reflex dynamic_node when: is_traffic_signal {
		counter <- counter + 1;
		if (counter >= time_to_change) {
			counter <- 0;
			if is_green {
				do to_red;
			} else {
				do to_green;
			}

		}

	}

	aspect default {
		if (display3D) {
			if (is_traffic_signal) {
				draw box(1, 1, 10) color: #black;
				draw sphere(3) at: {location.x, location.y, 10} color: color_fire;
			}
		} else {
			if (is_traffic_signal) {
				draw circle(5) color: color_fire;
			}
		}	
	}
}

//species that will represent the roads, it can be directed or not and uses the skill skill_road
species road skills: [skill_road] {
	geometry geom_display;
	string oneway;
	int hasAcc;
	int time_to_restart;

	aspect default {
		if (display3D) {
			draw geom_display color: #lightgray;
		} else {
			draw shape color: #white end_arrow: 5;
		}
	}
	
	action add_accident{
		hasAcc <- hasAcc + 1;
		maxspeed <- 0.0;
	}
	
	action remove_accident{
		hasAcc <- hasAcc - 1;
		if(hasAcc = 0){
			maxspeed <- (lanes = 1 ? 30.0 : (lanes = 2 ? 50.0 : 70.0)) °km / °h;
		}
	}

}

//People species that will move on the graph of roads to a target and using the driving skill
species people skills: [advanced_driving] {
	bool hasWaze ;
	rgb color <- rgb("orange");
	int counter_stucked <- 0;
	int threshold_stucked;
	intersection target;
	int time_to_restart <- 0;

	reflex time_to_go when: final_target = nil {
		target <- one_of(intersection );
		current_path <- compute_path(graph: road_network, target: target);
		if (current_path = nil) {
			location <- one_of(intersection).location;
		} 
	}

	reflex move when: current_path != nil and final_target != nil {
		do drive;
		if (final_target != nil) {
			if real_speed < 5 #km / #h {
				counter_stucked <- counter_stucked + 1;
				if (counter_stucked mod threshold_stucked = 0) {
					proba_use_linked_road <- min([1.0, proba_use_linked_road + 0.1]);
				}
	
			} else {
				counter_stucked <- 0;
				proba_use_linked_road <- 0.0;
			}
		}
	}

	aspect default {
		if (display3D) {
			point loc <- calcul_loc();
			draw rectangle(1,vehicle_length) + triangle(1) rotate: heading + 90 depth: 1 color: color at: loc;
		}else {
			draw triangle(8) color: color rotate: heading + 90;
		}
		
	}

	point calcul_loc {
		if (current_road = nil) {
			return location;
		} else {
			float val <- (road(current_road).lanes - current_lane) + 0.5;
			val <- on_linked_road ? -val : val;
			if (val = 0) {
				return location;
			} else {
				return (location + {cos(heading + 90) * val, sin(heading + 90) * val});
			}

		}

	} 
	
	action accident{
		color <- rgb("black");
		speed_coeff <- 0.0;
		time_to_restart <- rnd(1,100);
		//road(current_road): do add_accident;		
	}
	
	action restart_car{
		color <- hasWaze ? rgb("blue") : rgb("orange");
		speed_coeff <- 1.2 - (rnd(400) / 1000);
		//road(current_road): do remove_accident;		
	}
	
	reflex dynamic when: time_to_restart > 0 {
		time_to_restart <- time_to_restart - 1;
		if(time_to_restart = 0) {
			do restart_car;
		}
	}
	
	
	
}

experiment experiment_city type: gui {
	parameter "if true, 3D display, if false 2D display:" var: display3D category: "GIS";
	parameter "Nb accidents to add" var: n_accidents;
	
	action add_n_accidents{
		ask n_accidents among people{
			do accident;
		}
	}
	
	action _init_{
		create simulation with:[
			shape_file_roads::file("../includes/roads.shp"), 
			shape_file_nodes::file("../includes/nodes.shp"),
			nb_people::200
		];
	}
	
	user_command cmd_add_n_accidents action:add_n_accidents;
	
	output {
		display Main type: opengl synchronized: true background: #gray{
			species road ;
			species intersection ;
			species people ;
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

experiment experiment_ring type: gui {
	parameter "if true, 3D display, if false 2D display:" var: display3D category: "GIS";
	
	action _init_{
		create simulation with:[
			shape_file_roads::file("../includes/RoadCircleLanes.shp"), 
			shape_file_nodes::file("../includes/NodeCircleLanes.shp"),
			nb_people::20
		];
	}
	output {
		display Main type: opengl synchronized: true background: #gray{
			species road ;
			species intersection ;
			species people ;
		}

	}

}

