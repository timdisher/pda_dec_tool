// Global variables
var n = 4256;
var r_base = 5;
var r_mult = 15;

Highcharts.chart("bpd_net", {
    chart: {
        type: 'networkgraph',
        height: 'auto'
    },
    title: {text: "Bronchopulmonary Dysplasia"},
    subtitle: {text: "<em>32 trials; 2618 infants" +
      "<br>Treatments with no data: Ibuprofen (PO high dose) | Indomethacin (continuous IV)</em>",
      style: {"text-align": "center",
              "font-size" : '80%'}
      
    },
    plotOptions: {

        networkgraph: {
            layoutAlgorithm: {
            		linkLength: 100,
                enableSimulation: true,
                gravitationalConstant: 0.1
            }
        }
    },

    credits: {enabled: false},
    
    series: [{

        
        dataLabels: {
            enabled: true
        },
        
        nodes: [{
        	id: '1',
          name: "Placebo or no treatment (N = 333)",
          dataLabels: {
          	format: 'PBO'
          },
          marker:{
            radius: r_base * (1 + 333/n*r_mult)
          },
        },
        
        {
        	id: '2',
          name: "Indomethacin, IV (N = 810)",
          dataLabels: {
          	format: 'Indo (IV)'
          },
          marker:{
            radius: r_base * (1 + 810/n*r_mult)
          }
        }, 
        {
        	id: '3',
          name: "Ibuprofen, standard IV dose (N = 653)",
          dataLabels: {
          	format: 'Ibu (std IV)'
          },
          marker: {
            radius: r_base * (1 + 653/n*r_mult)
          },
        },
        {
        	id: '4',
          name: "Ibuprofen, standard oral dose (N = 363)",
          dataLabels: {
          	format: 'Ibu (std PO)'
          },
          marker: {
          	radius: r_base * (1 + 363/n*r_mult),
          },
        }, {
          id: '5',
          name: 'Acetaminophen, oral (N = 162)',
          dataLabels: {
            format: "Acet (PO)"
          },
          marker: {
            radius: r_base * (1 + 162/n*r_mult)
          }
        }, {
          id: '6',
          name: 'Ibuprofen, high IV dose (N = 35)',
          dataLabels: {
            format: "Ibu (high IV)"
          },
          marker: {
            radius: r_base * (1 + 35/n*r_mult)
          }
        }, {
          id: '7',
          name: 'Ibuprofen, continuous IV infusion (N = 55)',
          dataLabels: {
            format: "Ibu (cont IV)"
          },
          marker: {
            radius: r_base * (1 + 55/n*r_mult)
          }
        }, {
          id: '8',
          name: 'Indomethacin, other types (N = 207)',
          dataLabels: {
            format: "Indo other"
          },
          marker: {
            radius: r_base * (1 + 207/n*r_mult)
          }
        }
        ],
        data: [{
        	from: '1',
          to: '2',
          width: 5,
        }, {
        	from: '1',
          to: '3',
          width: 1,
        }, {
        	from: '1',
          to: '4',
          width: 1,
        }, {
          from: '1',
          to:'8',
          width: 1
        }, {
          from: '2',
          to:'3',
          width: 8
        }, {
          from: '2',
          to:'4',
          width: 2
        }, {
          from: '2',
          to:'5',
          width: 1
        }, {
          from: '2',
          to:'8',
          width: 4
        }, {
          from: '3',
          to:'4',
          width: 3
        }, {
          from: '3',
          to:'6',
          width: 1
        },{
          from: '3',
          to:'7',
          width: 1
        }, {
          from: '4',
          to:'5',
          width: 2
        }
        ]
    }],

    exporting: {
      buttons:{
        contextButton: {
        menuItems: ["printChart",'downloadPNG', 'downloadPDF', 'downloadSVG']}
      }
    }
});
