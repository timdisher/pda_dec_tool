// Global variables
var n = 4256;
var r_base = 5;
var r_mult = 15;

Highcharts.chart("pda_net", {
    chart: {
        type: 'networkgraph',
        height: 'auto'
    },
    
    legend: {enabled: true,
      labelFormat: "Test"
    },
    
    title: {text: "Patent Ductus Arteriosus Closure"},
    subtitle: {text: "<em>60 trials; 4256 infants" + "<br>Treatments with no data: NA</em>",
      style: {"text-align": "center",
              "font-size" : '80%'
      },
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
          name: "Placebo or no treatment (N = 597)",
          dataLabels: {
          	format: 'PBO'
          },
          marker:{
            radius: r_base * (1 + 597/n*r_mult)
          },
        },
        
        {
        	id: '2',
          name: "Indomethacin, IV (N = 1125)",
          dataLabels: {
          	format: 'Indo (IV)'
          },
          marker:{
            radius: r_base * (1 + 1125/n*r_mult)
          }
        }, 
        {
        	id: '3',
          name: "Ibuprofen, standard IV dose (N = 786)",
          dataLabels: {
          	format: 'Ibu (std IV)'
          },
          marker: {
            radius: r_base * (1 + 786/n*r_mult)
          },
        },
        {
        	id: '4',
          name: "Ibuprofen, standard oral dose (N = 650)",
          dataLabels: {
          	format: 'Ibu (std PO)'
          },
          marker: {
          	radius: r_base * (1 + 1305/n*r_mult),
          },
        }, {
          id: '5',
          name: 'Acetaminophen, oral (N = 267)',
          dataLabels: {
            format: "Acet (PO)"
          },
          marker: {
            radius: r_base * (1 + 267/n*r_mult)
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
          name: 'Ibuprofen, high oral dose (N = 122)',
          dataLabels: {
            format: "Ibu (high PO)"
          },
          marker: {
            radius: r_base * (1 + 122/n*r_mult)
          }
        }, {
          id: '8',
          name: 'Ibuprofen, continuous IV infusion (N = 55)',
          dataLabels: {
            format: "Ibu (cont IV)"
          },
          marker: {
            radius: r_base * (1 + 55/n*r_mult)
          }
        }, {
          id: '9',
          name: 'Indomethacin, continuous IV infusion (N = 58)',
          dataLabels: {
            format: "Indo (cont IV)"
          },
          marker: {
            radius: r_base * (1 + 58/n*r_mult)
          }
        }, {
          id: '10',
          name: 'Indomethacin, other types (N = 561)',
          dataLabels: {
            format: "Indo other"
          },
          marker: {
            radius: r_base * (1 + 561/n*r_mult)
          }
        }
        ],
        data: [{
        	from: '1',
          to: '2',
          width: 4,
        }, {
        	from: '1',
          to: '3',
          width: 1,
        }, {
        	from: '1',
          to: '4',
          width: 4,
        }, {
          from: '1',
          to:'10',
          width: 5
        }, {
          from: '2',
          to:'3',
          width: 12
        }, {
          from: '2',
          to:'4',
          width: 4
        }, {
          from: '2',
          to:'5',
          width: 1
        }, {
          from: '2',
          to:'9',
          width: 2
        }, {
          from: '2',
          to:'10',
          width: 10
        }, {
          from: '3',
          to:'4',
          width: 4
        }, {
          from: '3',
          to:'6',
          width: 1
        }, {
          from: '3',
          to:'8',
          width: 1
        }, {
          from: '3',
          to:'9',
          width: 1
        }, {
          from: '4',
          to:'5',
          width: 3
        }, {
          from: '4',
          to:'7',
          width: 2
        }, {
          from: '4',
          to:'10',
          width: 4
        }, {
          from: '5',
          to:'7',
          width: 1
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
