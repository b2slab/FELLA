function viewNetwork(fileToView,massData,massValues) {    
    var width = window.innerWidth,
    height = window.innerHeight-35,
    shiftKey, ctrlKey;

    var nodeGraph = null;
    var vnodeGraph = null;
    var xScale = d3.scale.linear()
    .domain([0,width]).range([0,width]);
    var yScale = d3.scale.linear()
    .domain([0,height]).range([0, height]);

    var divButtons = d3.select("body").append("div")
    .style("width", width)
    .style("height", 100)
    .attr("id","extendedC")

    var svg = d3.select("#d3_graph")
    .attr("tabindex", 1)
    .on("keydown.brush", keydown)
    .on("keyup.brush", keyup)
    .each(function() { this.focus(); })
    .append("svg")
    .attr("width", width)
    .attr("height", height);

    var zoomer = d3.behavior.zoom().
        scaleExtent([0.1,100]).
        x(xScale).
        y(yScale).
        on("zoomstart", zoomstart).
        on("zoom", redraw);

    function zoomstart() {
        node.each(function(d) {
            d.selected = false;
            d.previouslySelected = false;
        });
        node.classed("selected", false);
        node.style("stroke-width",1)
        node.classed("fixed", function(d) { d.fixed = true; });
    }

    function redraw() {
        vis.attr("transform",
                 "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")");
    }

    var brusher = d3.svg.brush()
    .x(xScale)
    .y(yScale)
    .on("brushstart", function(d) {
        node.each(function(d) { 
            d.previouslySelected = shiftKey && d.selected; });
    })
    .on("brush", function() {
        var extent = d3.event.target.extent();

        node.classed("selected", function(d) {
            return d.selected = d.previouslySelected ^
            (extent[0][0] <= d.x && d.x < extent[1][0]
             && extent[0][1] <= d.y && d.y < extent[1][1]);
        });
    })
    .on("brushend", function() {
        d3.event.target.clear();
        d3.select(this).call(d3.event.target);
    });

    var svg_graph = svg.append('svg:g')
    .call(zoomer)

    var rect = svg_graph.append('svg:rect')
    .attr('width', width)
    .attr('height', height)
    .attr('fill', 'transparent')
    .attr('stroke', 'transparent')
    .attr('stroke-width', 1)
    .attr("id", "zrect")

    var brush = svg_graph.append("g")
    .datum(function() { return {selected: false, previouslySelected: false}; })
    .attr("class", "brush");

    var vis = svg_graph.append("svg:g");
    var pcolor = ["#DC6767", "#C286FF", "#DCC067", "#9CBDD0", "#8DBC8F"];
    var color = d3.scale.ordinal().range(pcolor);
    var foldcolor = d3.scale.linear()
    .domain([-2.5, 0, 2.5])
    .range(["red", "white", "green"]);

    brush.call(brusher)
    .on("mousedown.brush", null)
    .on("touchstart.brush", null) 
    .on("touchmove.brush", null)
    .on("touchend.brush", null); 

    brush.select('.background').style('cursor', 'auto');
    
    var link = vis.append("g")
    .attr("class", "link")
    .selectAll("line");
    
    var node = vis.append("g")
    .selectAll("circle");
    
    var alllinks = vis.append("g")
    .attr("class", "link")
    .selectAll("line");
    
    var dataa = [[100,100], [150,150], [200,200]];
    var labels = true
    var hmdb_diseases = ["11-beta-hydroxylase deficiency","2-Aminoadipic aciduria","2-hydroxyglutaric aciduria","2-ketoadipic acidemia","2-Ketoglutarate dehydrogenase complex deficiency","21-hydroxylase deficiency","3-Hydroxy-3-methylglutaryl-CoA lyase deficiency","3-Hydroxyisobutyric aciduria","3-methyl-crotonyl-glycinuria","3-Methylglutaconic aciduria","3-Methylglutaconic aciduria type I","3-methylglutaconic aciduria type II","5-oxoprolinase deficiency"," Abetalipoproteinemia", "ACTH deficiency","Acute liver disease ","Acute liver failure","Acute myelogenous leukemia","Acute promyelocytic leukemia","Acute seizures","Addison's Disease","Adenosine deaminase deficiency","Adenylosuccinate lyase deficiency","Adrenal hypoplasia","Adrenoleukodystrophy","Adrenoleukodystrophy","Adrenomyeloneuropathy","AIDS","Alcoholism","Aldehyde dehydrogenase deficiency","Aldosterone biosynthesis defects","Alkaptonuria","Alpha-1-antitrypsin deficiency","Alpha-aminoadipic aciduria","Alzheimer's disease","Aminoacylase I deficiency","Amphetamine psychosis","Amyotrophic lateral sclerosis","Anemia","Anephric patients","Angina","Anorexia nervosa","Anoxia","Appendicitis","Argininemia","Argininosuccinic aciduria","Aromatic L-amino acid decarboxylase deficiency","Aspartylglucosaminuria","Asthma","Athyreosis","Atrophic gastritis","Autism","Autosomal dominant polycystic kidney disease","Bacterial infections","Bacterial meningitis","Benign gynecological diseases","Benzene exposure","Beta-ketothiolase deficiency","Beta-thalassemia","Beta-ureidopropionase deficiency","Biliary atresia","Biotinidase deficiency","Bipolar disorder","Bladder infections","Brain tumors","Cachexia","Cadmium exposure","Canavan disease","Cancer with metastatic bone disease","Carbamoyl Phosphate Synthetase Deficiency","Cardiac arrest","Cardiopulmonary bypass","Cardiopulmonary resuscitation","Carnitine-acylcarnitine translocase deficiency","Carnosinuria","Celiac disease","Cerebral vasospasm","Cerebrocortical degeneration","Cerebrotendinous xanthomatosis","Cervical cancer","Cervical myelopathy","Cholangiocarcinoma","Choledochal cysts","Cholelithiasis","Cholesteryl ester storage disease","Chronic active hepatitis","Chronic kidney disease","Chronic pancreatitis","Chronic progressive external ophthalmoplegia and Kearns-Sayre syndrome","Chronic renal failure","Cirrhosis","Citrullinemia type I","CNS infections","CNS tumors","Cobalamin malabsorption","Cognitive disorders","Colorectal cancer","Congenital adrenal hyperplasia","Continuous ambulatory peritoneal dialysis","Convulsion","Coronary artery disease","Coronary heart disease","Cortical myoclonus","Cresol poisoning IBS","Creutzfeldt-Jakob disease","Crigler-Najjar syndrome type I","Critical illnesses","Crohn's disease","Cushing's Syndrome","Cystathioninuria","Cystic fibrosis","Cystinosis ","Cystinuria","Cytochrome C oxidase deficiency","D-2-hydroxyglutaric aciduria","D-Glyceric acidemia","Degenerative disc disease","Dementia","Demyelinating polyneuropathy","Dengue fever","Depersonalization disorder","Dermal fibroproliferative disorder","Diabetes mellitus type 1","Diabetes mellitus type 2","Dihydropyrimidinase deficiency","Dihydropyrimidine dehydrogenase deficiency","Dimethyl sulfide poisoning","Dimethyl sulfone supplementation","Diverticular disease","Eczema","Encephalitis","Endometrial cancer","Epilepsy","Essential hypertension","Ethanol intoxication","Ethylene glycol poisoning","Ethylmalonic encephalopathy","Eucalyptol exposure","Fabry disease","Familial mediterranean fever","Fanconi syndrome","Fatty Acid Oxidation disorder","Febrile seizures","Folate deficiency","Formic acid intoxication","Friedreich's ataxia","Fumarase deficiency","Functional hypothalamic amenorrhea","Gaba-transaminase deficiency ","Galactose-1-phosphate uridyltransferase deficiency","Galactosemia type 1","Gallbladder disease","Gamma-glutamyltransferase deficiency","Gestational diabetes","Glucagon deficiency","Glucocorticoid deficiency, familial isolated","Glucose transporter type 1 deficiency syndrome","Glutaric acidemia type 2","Glutaric aciduria II","Glutaric aciduria type 1","Glutaryl-CoA dehydrogenase deficiency (GDHD)","Glutathione synthetase deficiency","Glycerol intolerance syndrome","Glycerol kinase deficiency","Glycine N-methyltransferase deficiency","Glycogen storage disease","Glycolic aciduria","Gout","Growth hormone deficiency","Hartnup disease","Hawkinsinuria","Head injury","Headache","Heart failure","Heat stress","Hemodialysis","Hemolytic uremic syndrome","Hepatic and biliary malignancies","Hepatic coma","Hepatic encephalopathy","Hepatitis","Hepatobiliary diseases","Hepatocellular carcinoma","Hereditary coproporphyria","Hereditary spastic paraplegia","Herpes zoster","Histidinemia","Homocystinuria","Homocystinuria due to defect of N(5,10)-methylene THF deficiency","Homozygous sickle cell disease","Huntington's disease","Hydrocephalus","Hydrogen sulfide poisoning","Hydroxyprolinemia","Hyper beta-alaninemia","Hyperargininemia","Hypercholesterolemia","Hyperdibasic aminoaciduria I","Hyperlipidemia","Hypermethioninemia","Hyperoxalemia","Hyperthyroidism","Hypervalinemia","Hyperzincaemia and hypercalprotectinaemia","Hypobetalipoproteinemia","Hypogonadism","Hypophosphatemia","Hypothyroidism","Hypoxic-ischemic encephalopathy","Idiopathic intracranial hypertension","Idiopathic oro-facial pain","Idiopathic polyneuritis","Ileocystoplasty","Ileostomy","Iminoglycinuria","Impaired glucose tolerance","Infantile Refsum's disease","Intestinal failure","Intrahepatic biliary hypoplasia","Intraventricular hemorrhage","Invasive candidiasis","Irritable bowel syndrome","Ischemic heart disease","Isopropyl alcohol poisoning","Isovaleric acidemia","Juvenile myoclonic epilepsy","Ketosis","Kidney cancer","Kidney disease","Kidney transplantation","Kynureninase deficiency","L-2-hydroxyglutaric aciduria","Leigh's syndrome, subacute necrotizing encephalopathy","Lesch-Nyhan syndrome","Leukemia","Leukoencephalopathy","Lipid peroxidation","Liver disease","Long chain acyl-CoA dehydrogenase deficiency (LCAD)","Long-chain-3-hydroxyacyl CoA dehydrogenase deficiency","Lung Cancer","Lysinuric protein intolerance","Macular degeneration","Major depressive disorder","Malaria","Malonyl-Coa decarboxylase deficiency","Maple syrup urine disease","Mastocytosis","Meckels diverticulum","Medium Chain Acyl-CoA Dehydrogenase Deficiency","Melanoma","Meningitis","Menkes disease","Menstrual cycle","Metachromatic leukodystrophy","Methanol poisoning","Methionine adenosyltransferase deficiency","Methotrexate treatment","Methyl formate exposure","Methylenetetrahydrofolate reductase deficiency","Methylmalonic acidemia","Migraine","Mild cognitive impairment","Molybedum cofactor deficiency","Motor neuron disease","Mucopolysaccharidosis IVA","Multi-infarct dementia","Multiple acyl-CoA dehydrogenase deficiency","Multiple myeloma","Multiple sclerosis","Multiple system atrophy","Mycobacterium tuberculosis","Myocardial infarction","N-acetylglutamate synthetase deficiency","Narcolepsy","Neonatal hepatitis","Nephrotic syndrome","Neuroblastoma","Neuroborreliosis","Neurodegenerative disease","Neuroinfection","Obesity","Olivopontocerebral atrophy","Ornithine transcarbamylase deficiency","Orotic aciduria I","Osteoporosis","Ovarian cancer","Oxidative stress","Paget's disease","Pancreatic cancer","Panic disorder","Paraquat poisoning","Parkinson's disease","Pellagra","Pentosuria","Perillyl alcohol administration for cancer treatment","Peripheral neuropathy","Peripheral vascular disease","Peritoneal dialysis","Peroxisomal biogenesis defect","Peroxisomal disorders, new type, liver","Phenylketonuria","Pheochromocytoma","Polycystic ovary syndrome","Porphyria","Portal vein obstruction","Postpartum depression","Pregnancy","Pregnene hydroxylation deficiency ","Premenstrual dysphoric disorder","Prepartum depression","Preterm birth","Primary biliary cirrhosis","Primary hyperoxaluria","Primary hyperoxaluria I","Primary hyperoxaluria II","Primary hypomagnesemia","Prolactinoma","Prolidase deficiency","Propionic acidemia","Prostate cancer","Proteinuria","Purine nucleoside phosphorylase deficiency","Pyruvate dehydrogenase deficiency","Quetiapine poisoning","Rachialgia","Refractory localization-related epilepsy","Refsum's disease","Renal tubular acidosis, distal, RTA type 1","Rett syndrome","Reye's syndrome","Rhabdomyolysis","Rheumatoid arthritis","Rhinitis","Ribose-5-phosphate isomerase deficiency","Salla disease","Sarcoma","Sarcosinemia","Schistosomiasis","Schizophrenia","Segawa Syndrome","Sepiapterin reductase deficiency","Sepsis","Septic shock","Short bowel syndrome","Short Chain Acyl-Coa Dehydrogenase Deficiency","Sickle cell anemia","Sitosterolemia","Sjögren-Larsson syndrome","Small bowel bacterial overgrowth syndrome","Small intestinal malabsorption","Smith-Lemli-Opitz syndrome","Smoking","Sodium nitrate consumption","Spina Bifida","Spinal disc herniation","Spinal stenosis","Stomach cancer","Stress","Stroke","Subarachnoid hemorrhage","Succinic semialdehyde dehydrogenase deficiency","Sulfite oxidase deficiency","Supradiaphragmatic malignancy","tert-Amyl-methyl ether exposed","Testicular adrenal rest tumors","Tetrahydrofuran exposure","Thymidine treatment","Thyroid cancer","Tic disorder","Transaldolase deficiency","Transcobalamin II deficiency","Transurethral resection of the prostate","Trauma","Traumatic brain injury","Trimethylaminuria","Tuberculosis","Tuberculous meningitis","Tyrosinemia","Tyrosinemia I","Uremia","Very Long Chain Acyl-CoA Dehydrogenase Deficiency","Vessel occlusion","Viral infection","Vitamin B12 deficiency","Vitiligo","Wilson's disease","Xanthinuria type 1"]
    var bnodes = new Array();
    var filterApplied = false
    
    divButtons.append("button").text("Show/Hide Labels").attr("float","left").on("click", showLabels).attr("id","SHLabels")
    divButtons.append("button").text("Search Node").on("click", searchNode).attr("id","SNode")
    divButtons.append("button").text("Show Path").on("click", shortestPath).attr("id","SPath")
    divButtons.append("button").text("Disease Filter").on("click",filterDisease).attr("id","DFilter")
    divButtons.append("button").text("Biofluid Filter").on("click",filterBiofluid).attr("id","BFilter")
    divButtons.append("button").text("Save Subgraph").on("click",saveSubgraph).attr("id","SSubgraph")
    
    
   function showLabels() {
      if (labels) { 
        d3.selectAll("text").remove()
        labels = false
      }
      else {
        if (massData!="") {
          function visibleNodes(element) {return massData.indexOf(element.name)!=-1}
          dataAsNodes = nodeGraph.nodes.filter(visibleNodes) 
        }
        else { dataAsNodes = nodeGraph.nodes } 
        var text = vis.append("g")
            .attr("class", "labels")
          .selectAll("text")
            .data(dataAsNodes)
          .enter().append("text")
            .attr("x", function(d) {
                if (typeof(d.nlinks)=="number") { return d.x-14-d.nlinks }
                else { return d.x-14 }
            })
            .attr("y", function(d) {
                if (typeof(d.nlinks)=="number") { return d.y-6-d.nlinks }
                else { return d.y-6 }
            })
            .text(function(d) { return d.name})
            .attr("font-size", "12px")
        labels = true     
      }
    }
    
    function searchNode() {
      filtered = window.prompt("Please, enter the identifier of the node you want to find.","C00181");  
	    if (filtered != null) {	
	      filtered = filtered.split("/")
		    dfiltered = filtered[0].trim()
      }
      var correctId = false
      node.each(function(d) {
        if (d.name==dfiltered) { correctId = true }
      })
      if (correctId) {
        document.getElementById("SSubgraph").disabled = true
        filterApplied = true
        node.style("fill",function(d) {
          if (d.name==dfiltered) { return "#FCF407" }
          else if (typeof(d.foldchange)=="number") { return foldcolor(d.foldchange); }
          else if (d.group=="Pathway") { return "#DC6767" } 
          else if (d.group=="Module") { return "#C286FF" }
          else if (d.group=="Enzyme") { return "#DCC067" }
          else if (d.group=="Reaction") { return "#9CBDD0" }
          else if (d.group=="Compound") { return "#8DBC8F" }
          else { return "#1313B5" }
        })
      }
      else {
        alert('No nodes with the identifier "'+dfiltered+'" in this graph')
      }
    }
    
    function shortestPath () {
      //comprobar que hay solo dos nodos seleccionados
      selectedNodes = node.filter(isSelected)
      function isSelected (element) { return element.selected }
      if (selectedNodes[0].length==2) {
        //encontrar camino más corto entre esos dos nodos
        var selectedSource = "1"
        var selectedGoal = "2"
        selectedNodes.each(function(d) {
          if (selectedSource=="1") { selectedSource = d.name }
          else { selectedGoal = d.name }
        })
        Q = node
        Q.each(function(d) {
            if (d.name==selectedSource) { d.dist = 0 }
            else { d.dist = 1000000 }
            d.prev = "undefined"
            d.neighbor = false
            d.visited = false
        })
        var uName = "hola"
        var uDist = 2000000
        var uPrev = "undefined"
        var found = false
        var visited = 0
        while (Q[0].length>visited && !found) {
          Q.each(function(d) { d.neighbor = false; })
          uDist = 2000000
          Q.each(function(d) {
            if (!d.visited && d.dist<uDist) {
              uDist = d.dist
              uName = d.name
	      uPrev = d.prev
              u = d
            }
          })
          Q.each(function(d) {
            if(d.name==uName) { d.visited = true
                                visited = visited + 1}
          })
          if (uName==selectedGoal) { 
            found = true
          }
          else {
            link.each(function(d) {
              Q.each(function(e) {
                if ((d.source.name==uName && d.target.name==e.name) || (d.source.name==e.name && d.target.name==uName)) { e.neighbor = true }
              })
            })
            Q.each(function(d) {
              if (d.neighbor) {
                alt = uDist + 1
                if (alt<d.dist) {
                  d.dist = alt
                  d.prev = uName
                } 
              }
            })
          }
        }
        if (found) {
	        NodePath = new Array();
	        NodePath.push(selectedGoal)
          uName = selectedGoal
          while (uPrev!="undefined") {
            Q.each(function(m) {
              if (m.name==uPrev) {
		            NodePath.push(m.name)                
		            uName = m.name
		            uPrev = m.prev
              }
            })
          }
	      link.attr("stroke", function(d) {
		      for (var j = 0; j<NodePath.length-1;j++) {
			      if ((d.source.name==NodePath[j] && d.target.name==NodePath[j+1]) || (d.target.name==NodePath[j] && d.source.name==NodePath[j+1])) { return "#000" }
            	}
          })
        }
        else { alert("There is no connexion between these two nodes.")}
      }
      else if (selectedNodes[0].length<2){alert("Please, select two nodes.")}
      else { alert ("Please, select only two nodes.")}
    }
    
    function filterDisease() {
      filtered = window.prompt("Please, enter a disease and a biofluid in the following format: disease/biofluid\nor only a disease if you do not want to filter by biofluid.\n\nBiofluids: blood, urine, saliva, cerebrospinal fluid, cellular cytoplasm, bile, pericardial effusion, amniotic fluid\n","alzheimer/blood");  
	    if (filtered != null) {	
		    filtered = filtered.split("/")
		    dfiltered = filtered[0].trim()
		    dfiltered = dfiltered.toLowerCase();
		    var matches = []
		    for (var i = 0; i < hmdb_diseases.length - 1; i++) {
			    var lwdis = hmdb_diseases[i].toLowerCase();
    			var disindex = lwdis.indexOf(dfiltered);
			    if (disindex>-1) {matches.push(hmdb_diseases[i])}
    		}
		    if (matches.length>1) { alert(matches.length+' diseases containing "'+dfiltered+'": \n\n'+matches) }
		    else if (matches.length<1) { alert('No diseases containing "'+dfiltered) }
		    else {
          var biofluid = false
  	      if (filtered.length>2) { alert("The supported formats to search diseases are: 'disease/biofluid' or 'disease'") }
		      else if (filtered.length==2) {
            bfiltered = filtered[1].trim()
			      bfiltered = bfiltered.toLowerCase();
			      if (bfiltered=="blood" || bfiltered=="urine" || bfiltered=="saliva" || bfiltered=="cerebrospinal fluid" || bfiltered=="cellullar cytoplasm" || bfiltered=="bile" || bfiltered=="pericardial effusion" || bfiltered=="amniotic fluid") { biofluid = true }
			      else {
				      alert('No biofluid called "'+filtered[1]+'".\nFilter will be applied only by disease.')
			      }
		      }
          document.getElementById("SSubgraph").disabled = false
          filterApplied = true
          d3.csv("table-hmdb.csv",function(d) {
  			    return {
             kegg_id: d.kegg_id,
             diseases: d.diseases
            };
			    }, function(error,rows) {
              node.style("fill",function(e) {
                hasdis = false
					      rows.forEach(function(h) {
                  if ((e.name==h.kegg_id && h.diseases.indexOf(matches[0])!=-1 && biofluid && h.diseases[h.diseases.indexOf(matches[0])+matches[0].length+1]==bfiltered[0].toUpperCase() && h.diseases[h.diseases.indexOf(matches[0])+matches[0].length+2]==bfiltered[1] && h.diseases[h.diseases.indexOf(matches[0])+matches[0].length+3]==bfiltered[2]) || (e.name==h.kegg_id && h.diseases.indexOf(matches[0])!=-1 && !biofluid)) { hasdis = true }//return "#990000"}///e.disease = 1 }
					      })
                if (hasdis) {return "#990000"}
                else {return "#808080"}
				      })
			    })
		    }
	    }
    }
    
    function filterBiofluid() {
      filtered = window.prompt("Please, enter one of the following biofluids: blood, urine, saliva, cerebrospinal fluid, cellular cytoplasm, bile, pericardial effusion, amniotic fluid\nto find which compounds are located there.","blood");  
      if (filtered != null) {	
		    filtered = filtered.trim()
		    bfiltered = filtered.toLowerCase();
        bfluids = ["blood","urine","saliva","cerebrospinal fluid","cellullar cytoplasm","bile","pericardial effusion","amniotic fluid"]
        if (bfluids.indexOf(bfiltered)==-1) {alert('No biofluid called "'+filtered+'".')}
        else {
          document.getElementById("SSubgraph").disabled = false
          filterApplied = true
          d3.csv("table-hmdb.csv",function(d) {
    		    return {
             kegg_id: d.kegg_id,
             biofluids: d.biofluids
            };
			    }, function(error,rows) {
              node.style("fill",function(e) {
                inbio = false
                rows.forEach(function(h) {
                  if (e.name==h.kegg_id && h.biofluids.toLowerCase().indexOf(bfiltered)!=-1) {inbio = true}
                })
                if (inbio) {return "#660066" }
                else {return "#808080"}
              })
			    })
        }
      }
    }
    
    
    function saveSubgraph() {
      jsonText = '{\n  "nodes":['
      var nodenames = new Array();
      node.each(function(d) {
        if (filterApplied) {
          if (this.style.fill=="rgb(153, 0, 0)" || this.style.fill=="rgb(102, 0, 102)") {
            jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Compound"},'
            nodenames.push(d.name)
          }
        }
        else {
          if (d.group=="Compound" || massData!="") {
            jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Compound"},'
            nodenames.push(d.name)
          }
          else if (d.group=="Pathway") {
            jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Pathway"},'
            nodenames.push(d.name)
          }
          else if (d.group=="Enzyme") {
            jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Enzyme"},'
            nodenames.push(d.name)
          }
          else if (d.group=="Reaction") {
            jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Reaction"},'
            nodenames.push(d.name)
          }
          else if (d.group=="Module") {
            jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Module"},'
            nodenames.push(d.name)
          }
          else {
            jsonText = jsonText+'\n     {"name":"'+d.name+'"},'
            nodenames.push(d.name)
          }
        }
      })
      jsonText = jsonText.substring(0,jsonText.length-1)
      jsonText=jsonText+'\n  ],\n  "links":['
      link.each(function(d) {
        indsource = nodenames.indexOf(d.source.name)
        indtarget = nodenames.indexOf(d.target.name)
	if (indsource!=-1 && indtarget!=-1) {
		if (typeof(d.value)=="number") {
          		jsonText = jsonText+'\n     {"source":'+indsource+',"target":'+indtarget+',"value":'+d.value+'},'
        	}
        	else {
          		jsonText = jsonText+'\n     {"source":'+indsource+',"target":'+indtarget+',"value":1},'
        	}
	}
      })
      jsonText = jsonText.substring(0,jsonText.length-1)
      jsonText = jsonText+'\n  ]\n}'
      var dref = document.createElement('a');
      dref.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(jsonText));
      dref.setAttribute('download', "network.json");
      dref.style.display = 'none';
      document.body.appendChild(dref);
      dref.click();
      document.body.removeChild(dref)
    }
    
    
    center_view = function() {
        // Center the view on the molecule(s) and scale it so that everything
        // fits in the window

        if (nodeGraph === null)
            return;

        var nodes = nodeGraph.nodes;

        //no molecules, nothing to do
        if (nodes.length === 0)
            return;

        // Get the bounding box
        min_x = d3.min(nodes.map(function(d) {return d.x;}));
        min_y = d3.min(nodes.map(function(d) {return d.y;}));

        max_x = d3.max(nodes.map(function(d) {return d.x;}));
        max_y = d3.max(nodes.map(function(d) {return d.y;}));


        // The width and the height of the graph
        mol_width = max_x - min_x;
        mol_height = max_y - min_y;

        // how much larger the drawing area is than the width and the height
        width_ratio = width / mol_width;
        height_ratio = height / mol_height;

        // we need to fit it in both directions, so we scale according to
        // the direction in which we need to shrink the most
        min_ratio = Math.min(width_ratio, height_ratio) * 0.8;

        // the new dimensions of the molecule
        new_mol_width = mol_width * min_ratio;
        new_mol_height = mol_height * min_ratio;

        // translate so that it's in the center of the window
        x_trans = -(min_x) * min_ratio + (width - new_mol_width) / 2;
        y_trans = -(min_y) * min_ratio + (height - new_mol_height) / 2;


        // do the actual moving
        vis.attr("transform",
                 "translate(" + [x_trans, y_trans] + ")" + " scale(" + min_ratio + ")");

                 // tell the zoomer what we did so that next we zoom, it uses the
                 // transformation we entered here
                 zoomer.translate([x_trans, y_trans ]);
                 zoomer.scale(min_ratio);

    };

    function dragended(d) {
        //d3.select(self).classed("dragging", false);
        node.filter(function(d) { return d.selected; })
        .each(function(d) { d.fixed &= ~6; })
    }
    
    d3.json(fileToView, function(error, graph) {
        document.getElementById("DFilter").disabled = true
        document.getElementById("BFilter").disabled = true
        if (massData=="") {
          document.getElementById("SSubgraph").disabled = true
        }
        hasCompounds = false
        i = 0
        while (i<graph.nodes.length && !hasCompounds) {
          if (graph.nodes[i].group=="Compound" || massData!="") {
              hasCompounds=true
              document.getElementById("DFilter").disabled = false
              document.getElementById("BFilter").disabled = false
          }
          i = i+1
        }
        
        nodeGraph = graph;
        if (massData!="") {
          function visibleNodes(element) {return massData.indexOf(element.name)!=-1}
          dataAsNodes = graph.nodes.filter(visibleNodes) 
        }
        else { dataAsNodes = graph.nodes }
        node = node.data(dataAsNodes).enter().append("circle")
              
        graph.links.forEach(function(d) {
            d.source = graph.nodes[d.source];
            d.target = graph.nodes[d.target];
        });
        
        if (massData!="") {
          function visibleLinks(element) {return massData.indexOf(element.source.name)!=-1 && massData.indexOf(element.target.name)!=-1}
          dataAsLinks = graph.links.filter(visibleLinks)
        }
        else { dataAsLinks = graph.links }
        link = link.data(dataAsLinks).enter().append("line")
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; })
        .attr("stroke-width",2);

        var force = d3.layout.force()
        .charge(function(d) {
          if (massData!="") { return -100 }
	  else if (link[0].length<node[0].length/2) { return -100 }
          else { return -600 }
        })
        .linkDistance(60)
        .nodes(dataAsNodes)
        .links(dataAsLinks)
        .size([width, height])
        .start();
        
  
        function dragstarted(d) {
            d3.event.sourceEvent.stopPropagation();
            if (!d.selected && !shiftKey) {
                // if this node isn't selected, then we have to unselect every other node
                node.classed("selected", function(p) { return p.selected =  p.previouslySelected = false; });
            }

            d3.select(this).classed("selected", function(p) { d.previouslySelected = d.selected; return d.selected = true; });

            node.filter(function(d) { return d.selected; })
            .each(function(d) { d.fixed |= 2; })
            .style("stroke-width",2)
            node.filter(function(d) { return !d.selected;})
            .style("stroke-width",1)
        }

        function dragged(d) {
            node.filter(function(d) { return d.selected; })
            .each(function(d) { 
                d.x += d3.event.dx;
                d.y += d3.event.dy;

                d.px += d3.event.dx;
                d.py += d3.event.dy;
            })

            force.resume();
        }
         
        function fcallback(element) {
          if (element.group==1) { return element }
          else { return; }
        }
         
        node.attr("r", function(d) { 
          if (massData!="") {return 4}
          else if (typeof(d.nlinks)=="number") { return 4+d.nlinks }
          else { return 4 }
        })
        .attr('title', function(d) { 
          iindx = massData.indexOf(d.name)
          return massValues[iindx]
        })
        .style("fill", function(d) { 
          if (massData!="") {return "#8DBC8F"}
          else if (typeof(d.foldchange)=="number") { return foldcolor(d.foldchange); }
          else if (d.group=="Pathway") { return "#DC6767" } 
          else if (d.group=="Module") { return "#C286FF" }
          else if (d.group=="Enzyme") { return "#DCC067" }
          else if (d.group=="Reaction") { return "#9CBDD0" }
          else if (d.group=="Compound") { return "#8DBC8F" }
          else { return "#1313B5" }
        })
        .attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; })
        .style("stroke","black")
        .on("dblclick",function(d) {
          d3.event.stopPropagation()
          if (d.group=="Pathway" || d.group=="Module" || d.group=="Enzyme" || d.group=="Reaction" || d.group=="Compound" || massData!="") {
            window.open("http://www.genome.jp/dbget-bin/www_bget?"+d.name,'_blank');
          }
          else if (typeof(d.foldchange)=="number") {
            window.open("http://www.ncbi.nlm.nih.gov/gene/?term="+d.name,'_blank');
          }  
        })
        .on("click", function(d) {
            if (d3.event.defaultPrevented) return;

            if (!shiftKey) {
                //if the shift key isn't down, unselect everything
                node.classed("selected", function(p) { return p.selected =  p.previouslySelected = false; })
            }

            // always select this node
            d3.select(this).classed("selected", d.selected = !d.previouslySelected);
        })
        

        .on("mouseup", function(d) {
            //if (d.selected && shiftKey) d3.select(this).classed("selected", d.selected = false);
        })
        .call(d3.behavior.drag()
              .on("dragstart", dragstarted)
              .on("drag", dragged)
              .on("dragend", dragended));
              
        var text = vis.append("g")
            .attr("class", "labels")
          .selectAll("text")
            .data(dataAsNodes)
          .enter().append("text")
            .text(function(d) { return d.name})
            .attr("x", function(d) {
                if (typeof(d.nlinks)=="number") { return d.x-14-d.nlinks }
                else { return d.x-14 }
            })
            .attr("y", function(d) {
                if (typeof(d.nlinks)=="number") { return d.y-6-d.nlinks }
                else { return d.y-6 }
            })
            .attr("font-size", "12px")

              function tick() {
                  link.attr("x1", function(d) { return d.source.x; })
                  .attr("y1", function(d) { return d.source.y; })
                  .attr("x2", function(d) { return d.target.x; })
                  .attr("y2", function(d) { return d.target.y; });

                  node.attr('cx', function(d) { return d.x })
                  .attr('cy', function(d) { return d.y });
                  text.attr("x", function(d) {
                      if (typeof(d.nlinks)=="number") { return d.x-14-d.nlinks }
                      else { return d.x-14 }
                  })
                  .attr("y", function(d) {
                      if (typeof(d.nlinks)=="number") { return d.y-6-d.nlinks }
                      else { return d.y-6 }
                  })
                };

              force.on("tick", tick);
                        
    });


    function keydown() {
        shiftKey = d3.event.shiftKey || d3.event.metaKey;
        ctrlKey = d3.event.ctrlKey;

        console.log('d3.event', d3.event)

        if (d3.event.keyCode == 67) {   //the 'c' key
            center_view();
        }

        if (shiftKey) {
            svg_graph.call(zoomer)
            .on("mousedown.zoom", null)
            .on("touchstart.zoom", null)                                                                      
            .on("touchmove.zoom", null)                                                                       
            .on("touchend.zoom", null);                                                                       

            //svg_graph.on('zoom', null);                                                                     
            vis.selectAll('g.gnode')
            .on('mousedown.drag', null);

            brush.select('.background').style('cursor', 'crosshair')
            brush.call(brusher);
        }
    }

    function keyup() {
        shiftKey = d3.event.shiftKey || d3.event.metaKey;
        ctrlKey = d3.event.ctrlKey;

        brush.call(brusher)
        .on("mousedown.brush", null)
        .on("touchstart.brush", null)                                                                      
        .on("touchmove.brush", null)                                                                       
        .on("touchend.brush", null);                                                                       

        brush.select('.background').style('cursor', 'auto')
        svg_graph.call(zoomer);
    }
}


function viewClusters(fileToView) {
  var width = window.innerWidth,
    height = window.innerHeight-35,
    shiftKey, ctrlKey;

    var nodeGraph = null;
    var xScale = d3.scale.linear()
    .domain([0,width]).range([0,width]);
    var yScale = d3.scale.linear()
    .domain([0,height]).range([0, height]);
    
    var divButtons = d3.select("body").append("div")
    .style("width", width)
    .style("height", 100)

    var svg = d3.select("#d3_graph")
    .attr("tabindex", 1)
    .on("keydown.brush", keydown)
    .on("keyup.brush", keyup)
    .each(function() { this.focus(); })
    .append("svg")
    .attr("width", width)
    .attr("height", height);

    var zoomer = d3.behavior.zoom().
        scaleExtent([0.1,100]).
        x(xScale).
        y(yScale).
        on("zoomstart", zoomstart).
        on("zoom", redraw);

    function zoomstart() {
        node.each(function(d) {
            d.selected = false;
            d.previouslySelected = false;
        });
        node.classed("selected", false);
        node.classed("fixed", function(d) { d.fixed = true; });
    }
    
    var prevscale = 1,
	prevtransx = 0,
	prevtransy = 0,
	prevgroup = "C1",
	xmin = 0,
	xmax = 1250,
	ymin = 0,
	ymax = 1050,
	newScale = "C1",
  gfiltered = false;

    function redraw() {
        vis.attr("transform",
                 "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")");
        if (!gfiltered) {
          xmin = -d3.event.translate[0]/d3.event.scale
  xmax = (width-d3.event.translate[0])/d3.event.scale
	ymin = -d3.event.translate[1]/d3.event.scale
	ymax = (height-d3.event.translate[1])/d3.event.scale
        function A(param1,callback) {
            d3.selectAll("circle").remove()
            d3.selectAll("line").remove()
            callback();
        }
        if (d3.event.scale<1.6 && prevscale>=1.6) { 
	  prevtransx = d3.event.translate[0]
	  prevgroup = "C1"
          A("h",function() {drawing("C1",0,1250,0,1050);})
          A("h",function() {drawing("C1",0,1250,0,1050);})
        }
        else if (d3.event.scale>=1.6 && d3.event.scale<2.7  && (prevscale<1.6 || prevscale>=2.7)) { 
          prevtransx = d3.event.translate[0]
	  prevgroup = "C2"
          A("h",function() {drawing("C2",xmin,xmax,ymin,ymax);})
          A("h",function() {drawing("C2",xmin,xmax,ymin,ymax);}) 
        }
        else if (d3.event.scale>=2.7 && d3.event.scale<3.6 && (prevscale<2.7 || prevscale>=3.6)) { 
          prevtransx = d3.event.translate[0]
	  prevgroup = "C3"
          A("h",function() {drawing("C3",xmin,xmax,ymin,ymax);}) 
          A("h",function() {drawing("C3",xmin,xmax,ymin,ymax);}) 
        }
        else if (d3.event.scale>=3.6 && prevscale<3.6) {
          prevtransx = d3.event.translate[0]
	  prevgroup = "Compound"
          A("h",function() {drawing("Compound",xmin,xmax,ymin,ymax);}) 
          A("h",function() {drawing("Compound",xmin,xmax,ymin,ymax);}) 
        }
        prevscale = d3.event.scale
        }
	
    }

    var brusher = d3.svg.brush()
    .x(xScale)
    .y(yScale)
    .on("brushstart", function(d) {
        node.each(function(d) { 
            d.previouslySelected = shiftKey && d.selected; });
    })
    .on("brush", function() {
        var extent = d3.event.target.extent();

        node.classed("selected", function(d) {
            return d.selected = d.previouslySelected ^
            (extent[0][0] <= d.xi && d.xi < extent[1][0]
             && extent[0][1] <= d.yi && d.yi < extent[1][1]);
        });
    })
    .on("brushend", function() {
        d3.event.target.clear();
        d3.select(this).call(d3.event.target);
    });

    var svg_graph = svg.append('svg:g')
    .call(zoomer)

    var rect = svg_graph.append('svg:rect')
    .attr('width', width)
    .attr('height', height)
    .attr('fill', 'transparent')
    .attr('stroke', 'transparent')
    .attr('stroke-width', 1)
    .attr("id", "zrect")

    var brush = svg_graph.append("g")
    .datum(function() { return {selected: false, previouslySelected: false}; })
    .attr("class", "brush");

    var vis = svg_graph.append("svg:g");
    var pcolor = ["#FF5C0A","#DC6767", "#C286FF", "#DCC067", "#9CBDD0", "#8DBC8F"];
    var color = d3.scale.ordinal().range(pcolor);
    var plcolor = ["#FFFFFF","#E2E2E2","#D7D7D7","#C9C6C6","#BCBABA","#B5B3B3","#ACAAAA","#A6A5A5","#9E9D9D","#979696","#8E8D8D"]
    var linkcolor = d3.scale.ordinal().range(plcolor)
    var ccolor = ["#1f77b4","#ff7f0e","#2ca02c","#d62728","#8A2BE2","#8c564b","#e377c2","#bcbd22","#17becf","#FFD700","#aec7e8","#ffbb78","#98df8a","#ff9896","#c5b0d5","#c49c94","#f7b6d2","#dbdb8d","#9edae5","#FFFFCC"]
    var ccindx = 0
    var color1 = new Array(),
	color2 = new Array(),
	color3 = new Array(),
	color4 = new Array(),
	color5 = new Array(),
	color6 = new Array(),
	color7 = new Array(),
	color8 = new Array(),
	color9 = new Array(),
	color10 = new Array(),
	color11 = new Array(),
	color12 = new Array(),
	color13 = new Array(),
	color14 = new Array(),
	color15 = new Array(),
	color16 = new Array(),
	color17 = new Array(),
	color18 = new Array(),
	color19 = new Array(),
	color20 = new Array();

    brush.call(brusher)
    .on("mousedown.brush", null)
    .on("touchstart.brush", null) 
    .on("touchmove.brush", null)
    .on("touchend.brush", null); 

    brush.select('.background').style('cursor', 'auto');
    
    var link = vis.append("g")
    .attr("class", "link")
    .selectAll("line");
    
    var node = vis.append("g")
    .attr("class", "node")
    .selectAll("circle")
    
    var dataAsNodes = new Array();
    var dataAsLinks = new Array();
    
    var alllinks = vis.append("g")
    .attr("class", "link")
    .selectAll("line");
    
    
    var hmdb_diseases = ["11-beta-hydroxylase deficiency","2-Aminoadipic aciduria","2-hydroxyglutaric aciduria","2-ketoadipic acidemia","2-Ketoglutarate dehydrogenase complex deficiency","21-hydroxylase deficiency","3-Hydroxy-3-methylglutaryl-CoA lyase deficiency","3-Hydroxyisobutyric aciduria","3-methyl-crotonyl-glycinuria","3-Methylglutaconic aciduria","3-Methylglutaconic aciduria type I","3-methylglutaconic aciduria type II","5-oxoprolinase deficiency"," Abetalipoproteinemia", "ACTH deficiency","Acute liver disease ","Acute liver failure","Acute myelogenous leukemia","Acute promyelocytic leukemia","Acute seizures","Addison's Disease","Adenosine deaminase deficiency","Adenylosuccinate lyase deficiency","Adrenal hypoplasia","Adrenoleukodystrophy","Adrenoleukodystrophy","Adrenomyeloneuropathy","AIDS","Alcoholism","Aldehyde dehydrogenase deficiency","Aldosterone biosynthesis defects","Alkaptonuria","Alpha-1-antitrypsin deficiency","Alpha-aminoadipic aciduria","Alzheimer's disease","Aminoacylase I deficiency","Amphetamine psychosis","Amyotrophic lateral sclerosis","Anemia","Anephric patients","Angina","Anorexia nervosa","Anoxia","Appendicitis","Argininemia","Argininosuccinic aciduria","Aromatic L-amino acid decarboxylase deficiency","Aspartylglucosaminuria","Asthma","Athyreosis","Atrophic gastritis","Autism","Autosomal dominant polycystic kidney disease","Bacterial infections","Bacterial meningitis","Benign gynecological diseases","Benzene exposure","Beta-ketothiolase deficiency","Beta-thalassemia","Beta-ureidopropionase deficiency","Biliary atresia","Biotinidase deficiency","Bipolar disorder","Bladder infections","Brain tumors","Cachexia","Cadmium exposure","Canavan disease","Cancer with metastatic bone disease","Carbamoyl Phosphate Synthetase Deficiency","Cardiac arrest","Cardiopulmonary bypass","Cardiopulmonary resuscitation","Carnitine-acylcarnitine translocase deficiency","Carnosinuria","Celiac disease","Cerebral vasospasm","Cerebrocortical degeneration","Cerebrotendinous xanthomatosis","Cervical cancer","Cervical myelopathy","Cholangiocarcinoma","Choledochal cysts","Cholelithiasis","Cholesteryl ester storage disease","Chronic active hepatitis","Chronic kidney disease","Chronic pancreatitis","Chronic progressive external ophthalmoplegia and Kearns-Sayre syndrome","Chronic renal failure","Cirrhosis","Citrullinemia type I","CNS infections","CNS tumors","Cobalamin malabsorption","Cognitive disorders","Colorectal cancer","Congenital adrenal hyperplasia","Continuous ambulatory peritoneal dialysis","Convulsion","Coronary artery disease","Coronary heart disease","Cortical myoclonus","Cresol poisoning IBS","Creutzfeldt-Jakob disease","Crigler-Najjar syndrome type I","Critical illnesses","Crohn's disease","Cushing's Syndrome","Cystathioninuria","Cystic fibrosis","Cystinosis ","Cystinuria","Cytochrome C oxidase deficiency","D-2-hydroxyglutaric aciduria","D-Glyceric acidemia","Degenerative disc disease","Dementia","Demyelinating polyneuropathy","Dengue fever","Depersonalization disorder","Dermal fibroproliferative disorder","Diabetes mellitus type 1","Diabetes mellitus type 2","Dihydropyrimidinase deficiency","Dihydropyrimidine dehydrogenase deficiency","Dimethyl sulfide poisoning","Dimethyl sulfone supplementation","Diverticular disease","Eczema","Encephalitis","Endometrial cancer","Epilepsy","Essential hypertension","Ethanol intoxication","Ethylene glycol poisoning","Ethylmalonic encephalopathy","Eucalyptol exposure","Fabry disease","Familial mediterranean fever","Fanconi syndrome","Fatty Acid Oxidation disorder","Febrile seizures","Folate deficiency","Formic acid intoxication","Friedreich's ataxia","Fumarase deficiency","Functional hypothalamic amenorrhea","Gaba-transaminase deficiency ","Galactose-1-phosphate uridyltransferase deficiency","Galactosemia type 1","Gallbladder disease","Gamma-glutamyltransferase deficiency","Gestational diabetes","Glucagon deficiency","Glucocorticoid deficiency, familial isolated","Glucose transporter type 1 deficiency syndrome","Glutaric acidemia type 2","Glutaric aciduria II","Glutaric aciduria type 1","Glutaryl-CoA dehydrogenase deficiency (GDHD)","Glutathione synthetase deficiency","Glycerol intolerance syndrome","Glycerol kinase deficiency","Glycine N-methyltransferase deficiency","Glycogen storage disease","Glycolic aciduria","Gout","Growth hormone deficiency","Hartnup disease","Hawkinsinuria","Head injury","Headache","Heart failure","Heat stress","Hemodialysis","Hemolytic uremic syndrome","Hepatic and biliary malignancies","Hepatic coma","Hepatic encephalopathy","Hepatitis","Hepatobiliary diseases","Hepatocellular carcinoma","Hereditary coproporphyria","Hereditary spastic paraplegia","Herpes zoster","Histidinemia","Homocystinuria","Homocystinuria due to defect of N(5,10)-methylene THF deficiency","Homozygous sickle cell disease","Huntington's disease","Hydrocephalus","Hydrogen sulfide poisoning","Hydroxyprolinemia","Hyper beta-alaninemia","Hyperargininemia","Hypercholesterolemia","Hyperdibasic aminoaciduria I","Hyperlipidemia","Hypermethioninemia","Hyperoxalemia","Hyperthyroidism","Hypervalinemia","Hyperzincaemia and hypercalprotectinaemia","Hypobetalipoproteinemia","Hypogonadism","Hypophosphatemia","Hypothyroidism","Hypoxic-ischemic encephalopathy","Idiopathic intracranial hypertension","Idiopathic oro-facial pain","Idiopathic polyneuritis","Ileocystoplasty","Ileostomy","Iminoglycinuria","Impaired glucose tolerance","Infantile Refsum's disease","Intestinal failure","Intrahepatic biliary hypoplasia","Intraventricular hemorrhage","Invasive candidiasis","Irritable bowel syndrome","Ischemic heart disease","Isopropyl alcohol poisoning","Isovaleric acidemia","Juvenile myoclonic epilepsy","Ketosis","Kidney cancer","Kidney disease","Kidney transplantation","Kynureninase deficiency","L-2-hydroxyglutaric aciduria","Leigh's syndrome, subacute necrotizing encephalopathy","Lesch-Nyhan syndrome","Leukemia","Leukoencephalopathy","Lipid peroxidation","Liver disease","Long chain acyl-CoA dehydrogenase deficiency (LCAD)","Long-chain-3-hydroxyacyl CoA dehydrogenase deficiency","Lung Cancer","Lysinuric protein intolerance","Macular degeneration","Major depressive disorder","Malaria","Malonyl-Coa decarboxylase deficiency","Maple syrup urine disease","Mastocytosis","Meckels diverticulum","Medium Chain Acyl-CoA Dehydrogenase Deficiency","Melanoma","Meningitis","Menkes disease","Menstrual cycle","Metachromatic leukodystrophy","Methanol poisoning","Methionine adenosyltransferase deficiency","Methotrexate treatment","Methyl formate exposure","Methylenetetrahydrofolate reductase deficiency","Methylmalonic acidemia","Migraine","Mild cognitive impairment","Molybedum cofactor deficiency","Motor neuron disease","Mucopolysaccharidosis IVA","Multi-infarct dementia","Multiple acyl-CoA dehydrogenase deficiency","Multiple myeloma","Multiple sclerosis","Multiple system atrophy","Mycobacterium tuberculosis","Myocardial infarction","N-acetylglutamate synthetase deficiency","Narcolepsy","Neonatal hepatitis","Nephrotic syndrome","Neuroblastoma","Neuroborreliosis","Neurodegenerative disease","Neuroinfection","Obesity","Olivopontocerebral atrophy","Ornithine transcarbamylase deficiency","Orotic aciduria I","Osteoporosis","Ovarian cancer","Oxidative stress","Paget's disease","Pancreatic cancer","Panic disorder","Paraquat poisoning","Parkinson's disease","Pellagra","Pentosuria","Perillyl alcohol administration for cancer treatment","Peripheral neuropathy","Peripheral vascular disease","Peritoneal dialysis","Peroxisomal biogenesis defect","Peroxisomal disorders, new type, liver","Phenylketonuria","Pheochromocytoma","Polycystic ovary syndrome","Porphyria","Portal vein obstruction","Postpartum depression","Pregnancy","Pregnene hydroxylation deficiency ","Premenstrual dysphoric disorder","Prepartum depression","Preterm birth","Primary biliary cirrhosis","Primary hyperoxaluria","Primary hyperoxaluria I","Primary hyperoxaluria II","Primary hypomagnesemia","Prolactinoma","Prolidase deficiency","Propionic acidemia","Prostate cancer","Proteinuria","Purine nucleoside phosphorylase deficiency","Pyruvate dehydrogenase deficiency","Quetiapine poisoning","Rachialgia","Refractory localization-related epilepsy","Refsum's disease","Renal tubular acidosis, distal, RTA type 1","Rett syndrome","Reye's syndrome","Rhabdomyolysis","Rheumatoid arthritis","Rhinitis","Ribose-5-phosphate isomerase deficiency","Salla disease","Sarcoma","Sarcosinemia","Schistosomiasis","Schizophrenia","Segawa Syndrome","Sepiapterin reductase deficiency","Sepsis","Septic shock","Short bowel syndrome","Short Chain Acyl-Coa Dehydrogenase Deficiency","Sickle cell anemia","Sitosterolemia","Sjögren-Larsson syndrome","Small bowel bacterial overgrowth syndrome","Small intestinal malabsorption","Smith-Lemli-Opitz syndrome","Smoking","Sodium nitrate consumption","Spina Bifida","Spinal disc herniation","Spinal stenosis","Stomach cancer","Stress","Stroke","Subarachnoid hemorrhage","Succinic semialdehyde dehydrogenase deficiency","Sulfite oxidase deficiency","Supradiaphragmatic malignancy","tert-Amyl-methyl ether exposed","Testicular adrenal rest tumors","Tetrahydrofuran exposure","Thymidine treatment","Thyroid cancer","Tic disorder","Transaldolase deficiency","Transcobalamin II deficiency","Transurethral resection of the prostate","Trauma","Traumatic brain injury","Trimethylaminuria","Tuberculosis","Tuberculous meningitis","Tyrosinemia","Tyrosinemia I","Uremia","Very Long Chain Acyl-CoA Dehydrogenase Deficiency","Vessel occlusion","Viral infection","Vitamin B12 deficiency","Vitiligo","Wilson's disease","Xanthinuria type 1"];
    var NtoPaint = new Array();
    var Nnew = new Array();
    var labels = false;
    var correctId = new Array();
    var filterApp = "none"
    var bfilterb = new Array();
    var dfilterd = new Array();
    
    
    divButtons.append("button").text("Search Node").attr("float","left").on("click", searchNode).attr("id","SNode")
    divButtons.append("button").text("Disease Filter").on("click",filterDisease).attr("id","DFilter")
    divButtons.append("button").text("Biofluid Filter").on("click",filterBiofluid).attr("id","BFilter")
    divButtons.append("button").text("Paint Clusters").on("click", paintClusters).attr("id","PClusters") 
    divButtons.append("button").text("Save Subgraph").on("click",saveSubgraph).attr("id","SSubgraph") 
    
    function searchNode() {
      filtered = window.prompt("Please, enter the identifier of the node you want to find.","C20686");  
      if (filtered != null) {	
	      filtered = filtered.split("/")
		    dfiltered = filtered[0].trim()
      }
      i = 0
      while (i<nodeGraph.nodes.length) {
        if (nodeGraph.nodes[i].name==dfiltered && nodeGraph.nodes[i].group=="Compound") { 
          if (nodeGraph.nodes[i].cluster.length==6) {
              c1 = nodeGraph.nodes[i].cluster.substring(0,2)
              c1 = ["Cluster",c1].join(" ");
              c2 = nodeGraph.nodes[i].cluster.substring(0,4)
              c2 = ["Cluster",c2].join(" ");
          }
          else if (nodeGraph.nodes[i].cluster.length==7) {
              c1 = nodeGraph.nodes[i].cluster.substring(0,3)
              c1 = ["Cluster",c1].join(" ");
              c2 = nodeGraph.nodes[i].cluster.substring(0,5)
              c2 = ["Cluster",c2].join(" ");
          }
          correctId.push(nodeGraph.nodes[i].name)
          correctId.push(c1)
          correctId.push(c2)
          correctId.push(nodeGraph.nodes[i].cluster)
        }
        i = i+1
      }
      if (correctId.length!=0) {
	  filterApp = "s"
          node.style("fill",function(d) {
          	if (correctId.indexOf(d.name)!=-1) { return "#FCF407" }//#DE603A"}
          	else if (d.name.substring(0,7)=="Cluster") { return "#010000"}//"#1313B5" }
  	    	else { return "#8DBC8F" }
          })
          node.style("fill-opacity",function(d) {
            if (correctId.indexOf(d.name)!=-1) { return 1 }
            else if (d.name.substring(0,7)=="Cluster") { return 0.5 }
          else { return 1 }
          })
          
      }
      else {
        alert('No nodes with the identifier "'+dfiltered+'" in this graph')
      }
    }    
    
    
    function paintClusters() {
	selectedNodes = node.filter(isSelected)
        function isSelected (element) { return element.selected && element.name.substring(0,7)=="Cluster" }
        if (selectedNodes[0].length==1) {
		filterApp = "none"
		selectedNodes.style("fill",function(d) { 
			if (ccindx<=19) {return ccolor[ccindx] }
			else { return "#010000" }
		})
    		.style("fill-opacity",function(d) { 
			if (ccindx<=19) {return 1 }
			else { return 0.5 }
		})
    		selectedNodes.each(function(d) { 
      			ser = d.name.substring(8)
      			ser = ser.split("-")
      		})
    		i = 0
		while (i<nodeGraph.nodes.length) {
		  if (nodeGraph.nodes[i].name.substring(0,7)=="Cluster") {	
        	        ser2 = nodeGraph.nodes[i].name.substring(8) 
        		ser2 = ser2.split("-")
        	  }
		  else { 
			if (nodeGraph.nodes[i].group=="C1") { ser2 = [""] }
			else { ser2 = nodeGraph.nodes[i].cluster.split("-") }
       		  }
                  if ((ser.length==1 && ser2[0]==ser[0]) || (ser.length==2 && ser2[0]==ser[0] && ser2[1]==ser[1]) || (ser.length==3 && ser2[0]==ser[0] && ser2[1]==ser[1] && ser2[2]==ser[2])) {
				if (ccindx==0) {color1.push(nodeGraph.nodes[i].name)}
				else if (ccindx==1) {color2.push(nodeGraph.nodes[i].name)}
				else if (ccindx==2) {color3.push(nodeGraph.nodes[i].name)}
				else if (ccindx==3) {color4.push(nodeGraph.nodes[i].name)}
				else if (ccindx==4) {color5.push(nodeGraph.nodes[i].name)}
				else if (ccindx==5) {color6.push(nodeGraph.nodes[i].name)}
				else if (ccindx==6) {color7.push(nodeGraph.nodes[i].name)}
				else if (ccindx==7) {color8.push(nodeGraph.nodes[i].name)}
				else if (ccindx==8) {color9.push(nodeGraph.nodes[i].name)}
				else if (ccindx==9) {color10.push(nodeGraph.nodes[i].name)}
				else if (ccindx==10) {color11.push(nodeGraph.nodes[i].name)}
				else if (ccindx==11) {color12.push(nodeGraph.nodes[i].name)}
				else if (ccindx==12) {color13.push(nodeGraph.nodes[i].name)}
				else if (ccindx==13) {color14.push(nodeGraph.nodes[i].name)}
				else if (ccindx==14) {color15.push(nodeGraph.nodes[i].name)}
				else if (ccindx==15) {color16.push(nodeGraph.nodes[i].name)}
				else if (ccindx==16) {color17.push(nodeGraph.nodes[i].name)}
				else if (ccindx==17) {color18.push(nodeGraph.nodes[i].name)}
				else if (ccindx==18) {color19.push(nodeGraph.nodes[i].name)}
				else if (ccindx==19) {color20.push(nodeGraph.nodes[i].name)}
		}
                i = i+1
              }
	      if (ccindx<19) { ccindx = ccindx+1 }
	      else { ccindx = 30 }
    	}
	else { alert("Please, select one cluster") }
    }
    
    
    
    
    function filterDisease() {
        filtered = window.prompt("Please, enter a disease and a biofluid in the following format: disease/biofluid\nor only a disease if you do not want to filter by biofluid.\n\nBiofluids: blood, urine, saliva, cerebrospinal fluid, cellular cytoplasm, bile, pericardial effusion, amniotic fluid\n","alzheimer/blood");  
	if (filtered != null) {	
		filtered = filtered.split("/")
		dfiltered = filtered[0].trim()
		dfiltered = dfiltered.toLowerCase();
		var matches = []
		for (var i = 0; i < hmdb_diseases.length - 1; i++) {
		    var lwdis = hmdb_diseases[i].toLowerCase();
    		    var disindex = lwdis.indexOf(dfiltered);
		    if (disindex>-1) {matches.push(hmdb_diseases[i])}
    		}
		if (matches.length>1) { alert(matches.length+' diseases containing "'+dfiltered+'": \n\n'+matches) }
		else if (matches.length<1) { alert('No diseases containing "'+dfiltered) }
		else {
          		var biofluid = false
  	      		if (filtered.length>2) { alert("The supported formats to search diseases are: 'disease/biofluid' or 'disease'") }
		      	else if (filtered.length==2) {
            			bfiltered = filtered[1].trim()
			      	bfiltered = bfiltered.toLowerCase();
			      	if (bfiltered=="blood" || bfiltered=="urine" || bfiltered=="saliva" || bfiltered=="cerebrospinal fluid" || bfiltered=="cellullar cytoplasm" || bfiltered=="bile" || bfiltered=="pericardial effusion" || bfiltered=="amniotic fluid") { biofluid = true }
				else { alert('No biofluid called "'+filtered[1]+'".\nFilter will be applied only by disease.')	}
		        }
          		document.getElementById("SSubgraph").disabled = false
          		filterApp = "d"
			dfilterd = new Array();
			d3.csv("table-hmdb.csv",function(d) {
    		    		return {
             				kegg_id: d.kegg_id,
             				diseases: d.diseases
            	    		};
	  		}, function(error,rows) {
              			i = 0
	      			while (i<nodeGraph.nodes.length) {
		  			rows.forEach(function(h) {
						if ((nodeGraph.nodes[i].group=="Compound" && nodeGraph.nodes[i].name==h.kegg_id && h.diseases.indexOf(matches[0])!=-1 && biofluid && h.diseases[h.diseases.indexOf(matches[0])+matches[0].length+1]==bfiltered[0].toUpperCase() && h.diseases[h.diseases.indexOf(matches[0])+matches[0].length+2]==bfiltered[1] && h.diseases[h.diseases.indexOf(matches[0])+matches[0].length+3]==bfiltered[2]) || (nodeGraph.nodes[i].group=="Compound" && nodeGraph.nodes[i].name==h.kegg_id && h.diseases.indexOf(matches[0])!=-1 && !biofluid)) {
							dfilterd.push(nodeGraph.nodes[i].name)
							dfilterd.push("Cluster "+nodeGraph.nodes[i].cluster)
							dfilterd.push("Cluster "+nodeGraph.nodes[i].cluster.split("-")[0]+"-"+nodeGraph.nodes[i].cluster.split("-")[1])
							dfilterd.push("Cluster "+nodeGraph.nodes[i].cluster.split("-")[0])
						}
        	  			})
		  			i = i+1
	      			}
	  		})
	  		window.setTimeout(function() {
				node.style("fill",function(e) {
                			if (dfilterd.indexOf(e.name)!=-1) {return "#990000" }
                			else {return "#808080"}
         			})
				.style("fill-opacity",1)
	  		},7000)
		}
      	    }
	}	          


    
    function filterBiofluid() {
      filtered = window.prompt("Please, enter one of the following biofluids: blood, urine, saliva, cerebrospinal fluid, cellular cytoplasm, bile, pericardial effusion, amniotic fluid\nto find which compounds are located there.","blood");  
      if (filtered != null) {	
		    filtered = filtered.trim()
		    bfiltered = filtered.toLowerCase();
        bfluids = ["blood","urine","saliva","cerebrospinal fluid","cellullar cytoplasm","bile","pericardial effusion","amniotic fluid"]
        if (bfluids.indexOf(bfiltered)==-1) {alert('No biofluid called "'+filtered+'".')}
        else {
          document.getElementById("SSubgraph").disabled = false
          filterApp = "b"
 	  bfilterb = new Array();
          d3.csv("table-hmdb.csv",function(d) {
    		    return {
             		kegg_id: d.kegg_id,
             		biofluids: d.biofluids
            	    };
	  }, function(error,rows) {
              i = 0
	      while (i<nodeGraph.nodes.length) {
		  rows.forEach(function(h) {
			if (nodeGraph.nodes[i].group=="Compound" && nodeGraph.nodes[i].name==h.kegg_id && h.biofluids.toLowerCase().indexOf(bfiltered)!=-1) {
				bfilterb.push(nodeGraph.nodes[i].name)
				bfilterb.push("Cluster "+nodeGraph.nodes[i].cluster)
				bfilterb.push("Cluster "+nodeGraph.nodes[i].cluster.split("-")[0]+"-"+nodeGraph.nodes[i].cluster.split("-")[1])
				bfilterb.push("Cluster "+nodeGraph.nodes[i].cluster.split("-")[0])
			}
        	  })
		  i = i+1
	      }
	  })
	  window.setTimeout(function() {
		node.style("fill",function(e) {
                	if (bfilterb.indexOf(e.name)!=-1) {return "#660066" }
                	else {return "#808080"}
         	})
		.style("fill-opacity",1)
	  },7000)
	}
      }
    }

    function saveSubgraph() {
      jsonText = '{\n  "nodes":['
      var nodenames = new Array();
      if (filterApp=="b") {
      	i = 0
      	while (i<nodeGraph.nodes.length) {
		if (nodeGraph.nodes[i].group!="C1" && nodeGraph.nodes[i].group!="C2" && nodeGraph.nodes[i].group!="C3" && bfilterb.indexOf(nodeGraph.nodes[i].name)!=-1) {
            		jsonText = jsonText+'\n     {"name":"'+nodeGraph.nodes[i].name+'","group":"Compound"},'
            		nodenames.push(nodeGraph.nodes[i].name)
          	}
		i = i+1
        }
      }
      else if (filterApp=="d") {
	i = 0
	while (i<nodeGraph.nodes.length) { 
		if (nodeGraph.nodes[i].group!="C1" && nodeGraph.nodes[i].group!="C2" && nodeGraph.nodes[i].group!="C3" && dfilterd.indexOf(nodeGraph.nodes[i].name)!=-1) {
	            jsonText = jsonText+'\n     {"name":"'+nodeGraph.nodes[i].name+'","group":"Compound"},'
            	    nodenames.push(nodeGraph.nodes[i].name)
		}
		i = i+1
        }
      }
      else {
        node.each(function(d) {  
		if (d.name.substring(0,7)!="Cluster" && d.name.substring(0,1)=="C") {
            		jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Compound"},'
            		nodenames.push(d.name)
          	}
          	else if (d.name.substring(0,3)=="hsa") {
            		jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Pathway"},'
            		nodenames.push(d.name)
          	}
          	else if (d.name.split(".").length==4) {
            		jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Enzyme"},'
            		nodenames.push(d.name)
          	}
          	else if (d.name.substring(0,1)=="R") {
            		jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Reaction"},'
            		nodenames.push(d.name)
          	}
          	else if (d.name.substring(0,1)=="M") {
            		jsonText = jsonText+'\n     {"name":"'+d.name+'","group":"Module"},'
            		nodenames.push(d.name)
          	}
        })
      }
      jsonText = jsonText.substring(0,jsonText.length-1)
      jsonText=jsonText+'\n  ],\n  "links":['
      i = 0
      while (i<nodeGraph.links.length) {
      	indsource = nodenames.indexOf(nodeGraph.links[i].source.name)
        indtarget = nodenames.indexOf(nodeGraph.links[i].target.name)
	if (indsource!=-1 && indtarget!=-1) {
		if (typeof(nodeGraph.links[i].value)=="number") {
          		jsonText = jsonText+'\n     {"source":'+indsource+',"target":'+indtarget+',"value":'+nodeGraph.links[i].value+'},'
        	}
        	else {
          		jsonText = jsonText+'\n     {"source":'+indsource+',"target":'+indtarget+',"value":1},'
        	}
	}
	i = i+1
      }
      jsonText = jsonText.substring(0,jsonText.length-1)
      jsonText = jsonText+'\n  ]\n}'
      var dref = document.createElement('a');
      dref.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(jsonText));
      dref.setAttribute('download', "network.json");
      dref.style.display = 'none';
      document.body.appendChild(dref);
      dref.click();
      document.body.removeChild(dref)
    }
    
    center_view = function() {
        // Center the view on the molecule(s) and scale it so that everything
        // fits in the window

        if (nodeGraph === null)
            return;

        var nodes = nodeGraph.nodes;

        //no molecules, nothing to do
        if (nodes.length === 0)
            return;

        // Get the bounding box
        min_x = d3.min(nodes.map(function(d) {return d.xi;}));
        min_y = d3.min(nodes.map(function(d) {return d.yi;}));

        max_x = d3.max(nodes.map(function(d) {return d.xi;}));
        max_y = d3.max(nodes.map(function(d) {return d.yi;}));


        // The width and the height of the graph
        mol_width = max_x - min_x;
        mol_height = max_y - min_y;

        // how much larger the drawing area is than the width and the height
        width_ratio = width / mol_width;
        height_ratio = height / mol_height;

        // we need to fit it in both directions, so we scale according to
        // the direction in which we need to shrink the most
        min_ratio = Math.min(width_ratio, height_ratio) * 0.8;

        // the new dimensions of the molecule
        new_mol_width = mol_width * min_ratio;
        new_mol_height = mol_height * min_ratio;

        // translate so that it's in the center of the window
        x_trans = -(min_x) * min_ratio + (width - new_mol_width) / 2;
        y_trans = -(min_y) * min_ratio + (height - new_mol_height) / 2;


        // do the actual moving
        vis.attr("transform",
                 "translate(" + [x_trans, y_trans] + ")" + " scale(" + min_ratio + ")");

                 // tell the zoomer what we did so that next we zoom, it uses the
                 // transformation we entered here
                 zoomer.translate([x_trans, y_trans ]);
                 zoomer.scale(min_ratio);

    };

    function dragended(d) {
        //d3.select(self).classed("dragging", false);
        node.filter(function(d) { return d.selected; })
        .each(function(d) { d.fixed &= ~6; })
    }
    
    d3.json(fileToView, function(error, graph) {
        document.getElementById("DFilter").disabled = true
        document.getElementById("BFilter").disabled = true
        hasCompounds = false
        i = 0
        while (i<graph.nodes.length && !hasCompounds) {
          if (graph.nodes[i].group=="Compound") {
              hasCompounds=true
              document.getElementById("DFilter").disabled = false
              document.getElementById("BFilter").disabled = false
          }
          i = i+1
        }
        nodeGraph = graph;
        dataAsNodes = graph.nodes.filter(visibleNodes)
        function visibleNodes(element) {return element.group=="C1"}
        node = node.data(dataAsNodes).enter().append("circle")
        //alert(node.length)
        graph.links.forEach(function(d) {
            d.source = graph.nodes[d.source];
            d.target = graph.nodes[d.target];
        });
        dataAsLinks = graph.links.filter(visibleLinks)
        function visibleLinks(element) { return element.source.group=="C1" && element.target.group=="C1" }
        link = link.data(dataAsLinks).enter().append("line")
        .attr("x1", function(d) { return d.source.xi; })
        .attr("y1", function(d) { return d.source.yi; })
        .attr("x2", function(d) { return d.target.xi; })
        .attr("y2", function(d) { return d.target.yi; })
        .attr("stroke",function(d) { return linkcolor(d.weight); })
        .attr("stroke-width",2);

        var force = d3.layout.force()
        .charge(-120)
        .linkDistance(150)
        .nodes(graph.nodes)
        .links(graph.links)
        .size([width, height])
        .start();
        
  
        function dragstarted(d) {
            d3.event.sourceEvent.stopPropagation();
            if (!d.selected && !shiftKey) {
                // if this node isn't selected, then we have to unselect every other node
                node.classed("selected", function(p) { return p.selected =  p.previouslySelected = false; });
            }

            d3.select(this).classed("selected", function(p) { d.previouslySelected = d.selected; return d.selected = true; });

            node.filter(function(d) { return d.selected; })
            .each(function(d) { d.fixed |= 2; })
            .style("stroke-width",2)
            node.filter(function(d) { return !d.selected;})
            .style("stroke-width",1)
        }

        function dragged(d) {
            node.filter(function(d) { return d.selected; })
            .each(function(d) { 
                d.xi += d3.event.dx;
                d.yi += d3.event.dy;

                d.px += d3.event.dx;
                d.py += d3.event.dy;
            })

            force.resume();
        }
         
         
        node.attr("r", function(d) {
          if (d.nnodes>=20 && d.nnodes<100) {return 20}
          else if (d.nnodes>=100 && d.nnodes<1000) {return 25}
          else if (d.nnodes>=1000) {return 30}
          else if (d.nnodes==1) {return 3}
	  else if (d.nnodes>=2 && d.nnodes<6) {return 5}
          else {return d.nnodes}
        })
        .attr('title', function(d) { return d.name })
        .style("fill-opacity",function(d) {
		if (d.name.substring(0,7)=="Cluster") { return 0.5 }
		else { return 1 }
	})
        .style("fill", function(d) {
		if (d.name.substring(0,7)=="Cluster") { return "#010000"}//"#1313B5" }
		else { return "#8DBC8F" }
	})
        .attr("cx", function(d) { return d.xi; })
        .attr("cy", function(d) { return d.yi; })
        .each(function(d) { d.fixed = true })
	.style("stroke","black")
        .on("dblclick",function(d) {
          d3.event.stopPropagation()
          if (d.name.substring(0,7)=="Cluster") {
            var belong = new Array();
            var cserie = d.name.substring(8).split("-")
            i = 0
            while (i<nodeGraph.nodes.length) {
              if (graph.nodes[i].group=="Compound") {
                nserie = graph.nodes[i].cluster.split("-")
                //alert(nserie)
                if ((cserie.length==1 && nserie[0]==cserie[0]) || (cserie.length==2 && nserie[0]==cserie[0] && nserie[1]==cserie[1]) || (cserie.length==3 && nserie[0]==cserie[0] && nserie[1]==cserie[1] && nserie[2]==cserie[2])) {
                  belong.push(graph.nodes[i].name)
                }
              }
              i = i+1
            }
            alert("This cluster contains the following "+belong.length+" compounds:\n\n"+belong)
            }
            else {
              window.open("http://www.genome.jp/dbget-bin/www_bget?"+d.name,'_blank');
            }
        })
        .on("click", function(d) {
            if (d3.event.defaultPrevented) return;

            if (!shiftKey) {
                //if the shift key isn't down, unselect everything
                node.classed("selected", function(p) { return p.selected =  p.previouslySelected = false; })
            }

            // always select this node
            d3.select(this).classed("selected", d.selected = !d.previouslySelected);
        })
        

        .on("mouseup", function(d) {
            //if (d.selected && shiftKey) d3.select(this).classed("selected", d.selected = false);
        })
        .call(d3.behavior.drag()
              .on("dragstart", dragstarted)
              .on("drag", dragged)
              .on("dragend", dragended));

              function tick() {
                  link.attr("x1", function(d) { return d.source.xi; })
                  .attr("y1", function(d) { return d.source.yi; })
                  .attr("x2", function(d) { return d.target.xi; })
                  .attr("y2", function(d) { return d.target.yi; });

                  node.attr('cx', function(d) { return d.xi; })
                  .attr('cy', function(d) { return d.yi; });
                };

              force.on("tick", tick);
                        
    });


    function keydown() {
        shiftKey = d3.event.shiftKey || d3.event.metaKey;
        ctrlKey = d3.event.ctrlKey;

        console.log('d3.event', d3.event)

        if (d3.event.keyCode == 67) {   //the 'c' key
            center_view();
        }

        if (shiftKey) {
            svg_graph.call(zoomer)
            .on("mousedown.zoom", null)
            .on("touchstart.zoom", null)                                                                      
            .on("touchmove.zoom", null)                                                                       
            .on("touchend.zoom", null);                                                                       

            //svg_graph.on('zoom', null);                                                                     
            vis.selectAll('g.gnode')
            .on('mousedown.drag', null);

            brush.select('.background').style('cursor', 'crosshair')
            brush.call(brusher);
        }
    }

    function keyup() {
        shiftKey = d3.event.shiftKey || d3.event.metaKey;
        ctrlKey = d3.event.ctrlKey;

        brush.call(brusher)
        .on("mousedown.brush", null)
        .on("touchstart.brush", null)                                                                      
        .on("touchmove.brush", null)                                                                       
        .on("touchend.brush", null);                                                                       

        brush.select('.background').style('cursor', 'auto')
        svg_graph.call(zoomer);
    }
    
    function drawing (newScale,xmin,xmax,ymin,ymax) {
      d3.json(fileToView, function(error, graph) {       
            nodeGraph = graph;
            dataAsNodes = graph.nodes.filter(visibleNodes)
            function visibleNodes(element) { return element.group==newScale && element.xi>=xmin && element.xi<=xmax && element.yi>=ymin && element.yi<=ymax }
        node = node.data(dataAsNodes).enter().append("circle")
              
        graph.links.forEach(function(d) {
            d.source = graph.nodes[d.source];
            d.target = graph.nodes[d.target];
        });

	dataAsLinks = graph.links.filter(visibleLinks)
        function visibleLinks(element) { return element.source.group==newScale && element.target.group==newScale && element.source.xi>=xmin && element.source.xi<=xmax && element.source.yi>=ymin && element.source.yi<=ymax && element.target.xi>=xmin && element.target.xi<=xmax && element.target.yi>=ymin && element.target.yi<=ymax }
        link = link.data(dataAsLinks).enter().append("line")
        .attr("x1", function(d) { return d.source.xi; })
        .attr("y1", function(d) { return d.source.yi; })
        .attr("x2", function(d) { return d.target.xi; })
        .attr("y2", function(d) { return d.target.yi; })
        .attr("stroke",function(d) { return linkcolor(d.weight); })
        .attr("stroke-width",2);
        
        function R (param1,callback) {
          d3.selectAll("line").remove()
          callback();
        }

        function lDraw(ldata) {
          link = link.data(ldata).enter().append("line")
          .attr("x1", function(d) { return d.source.xi; })
          .attr("y1", function(d) { return d.source.yi; })
          .attr("x2", function(d) { return d.target.xi; })
          .attr("y2", function(d) { return d.target.yi; })
          .attr("stroke",function(d) { return linkcolor(d.weight); })
          .attr("stroke-width",2);
        }

        var force = d3.layout.force()
        .charge(-120)
        .linkDistance(150)
        .nodes(graph.nodes)
        .links(graph.links)
        .size([width, height])
        .start();
        
  
        function dragstarted(d) {
            d3.event.sourceEvent.stopPropagation();
            if (!d.selected && !shiftKey) {
                // if this node isn't selected, then we have to unselect every other node
                node.classed("selected", function(p) { return p.selected =  p.previouslySelected = false; });
            }

            d3.select(this).classed("selected", function(p) { d.previouslySelected = d.selected; return d.selected = true; });

            node.filter(function(d) { return d.selected; })
            .each(function(d) { d.fixed |= 2; })
            .style("stroke-width",2)
            node.filter(function(d) { return !d.selected;})
            .style("stroke-width",1)
        }

        function dragged(d) {
            node.filter(function(d) { return d.selected; })
            .each(function(d) { 
                d.xi += d3.event.dx;
                d.yi += d3.event.dy;

                d.px += d3.event.dx;
                d.py += d3.event.dy;
            })

            force.resume();
        }
         
        node.attr("r", function(d) {
          if (d.nnodes>=20 && d.nnodes<100) {return 20}
          else if (d.nnodes>=100 && d.nnodes<1000) {return 25}
          else if (d.nnodes>=1000) {return 30}
          else if (d.nnodes==1) {return 3}
	        else if (d.nnodes>=2 && d.nnodes<6) {return 5}
          else {return d.nnodes}
        })
        .attr('title', function(d) { return d.name })
        .style("fill-opacity",function(d) {
	  if (filterApp=="s") {
          	if (correctId.indexOf(d.name)!=-1) { return 1 }
		else {return 0.5}
	  }
	  else if (filterApp=="b") { return 1 }
	  else if (filterApp=="d") { return 1 }
	  else {
		if (color1.indexOf(d.name)!=-1 || color2.indexOf(d.name)!=-1 || color3.indexOf(d.name)!=-1 || color4.indexOf(d.name)!=-1 || color5.indexOf(d.name)!=-1 || color6.indexOf(d.name)!=-1 || color7.indexOf(d.name)!=-1 || color8.indexOf(d.name)!=-1 || color9.indexOf(d.name)!=-1 || color10.indexOf(d.name)!=-1 || color11.indexOf(d.name)!=-1 || color12.indexOf(d.name)!=-1 || color13.indexOf(d.name)!=-1 || color14.indexOf(d.name)!=-1 || color15.indexOf(d.name)!=-1 || color16.indexOf(d.name)!=-1 || color17.indexOf(d.name)!=-1 || color18.indexOf(d.name)!=-1 || color19.indexOf(d.name)!=-1 || color20.indexOf(d.name)!=-1) { return 1 }
    else if (d.name.substring(0,7)=="Cluster") { return 0.5 }
		else { return 1 }
	  }
	})
        .style("fill", function(d) {
          if (filterApp=="s") {
		if (correctId.indexOf(d.name)!=-1) { return "#FCF407" }//#DE603A"}
          	else if (d.name.substring(0,7)=="Cluster") { return "#010000"}//"#1313B5" }
  	      	else { return "#8DBC8F" }
	  }
	  else if (filterApp=="b") {
		if (bfilterb.indexOf(d.name)!=-1) {return "#660066" }
                else {return "#808080"}
	  }
	  else if (filterApp=="d") {
		if (dfilterd.indexOf(d.name)!=-1) {return "#990000" }
                else {return "#808080"}
	  }
          else {
		if (color1.indexOf(d.name)!=-1) { return ccolor[0] }
		else if (color2.indexOf(d.name)!=-1) { return ccolor[1] }
		else if (color3.indexOf(d.name)!=-1) { return ccolor[2] }
		else if (color4.indexOf(d.name)!=-1) { return ccolor[3] }
		else if (color5.indexOf(d.name)!=-1) { return ccolor[4] }
		else if (color6.indexOf(d.name)!=-1) { return ccolor[5] }
		else if (color7.indexOf(d.name)!=-1) { return ccolor[6] }
		else if (color8.indexOf(d.name)!=-1) { return ccolor[7] }
		else if (color9.indexOf(d.name)!=-1) { return ccolor[8] }
		else if (color10.indexOf(d.name)!=-1) { return ccolor[9] }
		else if (color11.indexOf(d.name)!=-1) { return ccolor[10] }
		else if (color12.indexOf(d.name)!=-1) { return ccolor[11] }
		else if (color13.indexOf(d.name)!=-1) { return ccolor[12] }
		else if (color14.indexOf(d.name)!=-1) { return ccolor[13] }
		else if (color15.indexOf(d.name)!=-1) { return ccolor[14] }
		else if (color16.indexOf(d.name)!=-1) { return ccolor[15] }
		else if (color17.indexOf(d.name)!=-1) { return ccolor[16] }
		else if (color18.indexOf(d.name)!=-1) { return ccolor[17] }
		else if (color19.indexOf(d.name)!=-1) { return ccolor[18] }
		else if (color20.indexOf(d.name)!=-1) { return ccolor[19] }
		else if (d.name.substring(0,7)=="Cluster") { return "#010000"}//"#1313B5" }
		else { return "#8DBC8F" }
	  }
	})
	.attr("cx", function(d) { return d.xi; })
        .attr("cy", function(d) { return d.yi; })
        .each(function(d) { d.fixed = true })
	.style("stroke","black")
        .on("dblclick",function(d) {
          d3.event.stopPropagation()
          if (typeof(d.foldchange)=="number") {
            window.open("http://www.ncbi.nlm.nih.gov/gene/?term="+d.name,'_blank');
          } 
          else if (d.name.substring(0,7)=="Cluster") {
            var belong = new Array();
            var cserie = d.name.substring(8).split("-")
            i = 0
            while (i<nodeGraph.nodes.length) {
              if (graph.nodes[i].group=="Compound") {
                nserie = graph.nodes[i].cluster.split("-")
                if ((cserie.length==1 && nserie[0]==cserie[0]) || (cserie.length==2 && nserie[0]==cserie[0] && nserie[1]==cserie[1]) || (cserie.length==3 && nserie[0]==cserie[0] && nserie[1]==cserie[1] && nserie[2]==cserie[2])) {
                  belong.push(graph.nodes[i].name)
                }
              }
              i = i+1
            }
            alert("This cluster contains the following "+belong.length+" compounds:\n\n"+belong)
            }
            else {
              window.open("http://www.genome.jp/dbget-bin/www_bget?"+d.name,'_blank');
            }
        })
        .on("click", function(d) {
            if (d3.event.defaultPrevented) return;

            if (!shiftKey) {
                //if the shift key isn't down, unselect everything
                node.classed("selected", function(p) { return p.selected =  p.previouslySelected = false; })
            }

            // always select this node
            d3.select(this).classed("selected", d.selected = !d.previouslySelected);
        })
        

        .on("mouseup", function(d) {
            //if (d.selected && shiftKey) d3.select(this).classed("selected", d.selected = false);
        })
        .call(d3.behavior.drag()
              .on("dragstart", dragstarted)
              .on("drag", dragged)
              .on("dragend", dragended));

              function tick() {
                  link.attr("x1", function(d) { return d.source.xi; })
                  .attr("y1", function(d) { return d.source.yi; })
                  .attr("x2", function(d) { return d.target.xi; })
                  .attr("y2", function(d) { return d.target.yi; });

                  node.attr('cx', function(d) { return d.xi; })
                  .attr('cy', function(d) { return d.yi; });
                };

              force.on("tick", tick);                        
    });
    }
}
