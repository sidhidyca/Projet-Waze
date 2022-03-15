/**
 *  RoadTrafficComplex
 *  Author: patricktaillandier
 *  Description: 
 */
 
model RoadTrafficComplex
 
global {   
	file shape_file_roads  <- file("../includes/roads.shp") ;
	file shape_file_nodes  <- file("../includes/nodes.shp");
	geometry shape <- envelope(shape_file_roads);
	
	graph road_network;
	int nb_people <- 10000;
	int n_modified_roads <- 4000;
	float car_length <- 3 #m;
	float weight_added <- 100000.0;
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
	
	init {  
		create roadNode from: shape_file_nodes;
		create road from: shape_file_roads with:[lanes::int(read("lanes")), 
			maxspeed::float(read("maxspeed")) °km/°h, oneway::string(read("oneway"))
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
					}
				}
				match "-1" {
					shape <- polyline(reverse(shape.points));
				}
			}
		}	
		general_speed_map <- road as_map (each::(each.shape.perimeter / (each.maxspeed)));
		write "Number of road : " + length(road);
		write "Number of node : " + length(roadNode);
		write "Speed map : " + length(general_speed_map);
		road_network <-  (as_driving_graph(road, roadNode))  with_weights general_speed_map;
		
		create people number: nb_people { 
			speed <- 30 #km /#h ;
			vehicle_length <- car_length;
			right_side_driving <- true;
			proba_lane_change_up <- rnd(1.0);
			proba_lane_change_down <- rnd(0.5,1.0);
			location <- one_of(roadNode).location;
			security_distance_coeff <- rnd(1.0,3.0);
			proba_respect_priorities <- rnd(0.8,1.0);
			proba_respect_stops <- [rnd(0.998,1.0)];
			proba_block_node <- rnd(0.0,0.003);
			proba_use_linked_road <- 0.0;
			max_acceleration <- rnd(0.5,1.0);
			speed_coeff <- rnd(0.8,1.2);
		}	
	}
		
} 
species roadNode skills: [skill_road_node]{
		
	aspect geom3D {	
		draw sphere(5) at: {location.x,location.y,12} color: #green;
	}
}

species road skills: [skill_road] {
	int hasAcc; 
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
		put weight_added key:self in: general_speed_map;
		hasAcc <- 1;
		maxspeed <- 1.0;
	}
	
	user_command cmd_add_weight action:add_weight;
}
	
species people skills: [advanced_driving] { 
	rgb color <- rnd_color(255);
	roadNode target;
	
	reflex time_to_go when: final_target = nil {
		target <- one_of(roadNode);
		current_path <- compute_path(graph: road_network, target: target);
	}
	reflex move when: final_target != nil {
		do drive;
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
	
} 

experiment traffic_simulation type: gui {
	
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
	}
}