// Global variables
var n = 4256;
var r_base = 5;
var r_mult = 15;

Highcharts.chart("nec_net", {
    chart: {
        type: 'networkgraph',
        height: 'auto'
    },
    title: {text: "Necrotizing Enterocolitis"},
    subtitle: {text: "<em>45 trials; 3371 infants"+
      "<br>Treatments with no data: NA </em>",
      style: {"text-align": "center",
              "font-size" : '80%'},
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
          name: "Placebo or no treatment (N = 367)",
          dataLabels: {
          	format: 'PBO'
          },
          marker:{
            radius: r_base * (1 + 367/n*r_mult)
          },
        },
        
        {
        	id: '2',
          name: "Indomethacin, IV (N = 931)",
          dataLabels: {
          	format: 'Indo (IV)'
          },
          marker:{
            radius: r_base * (1 + 931/n*r_mult)
          }
        }, 
        {
        	id: '3',
          name: "Ibuprofen, standard IV dose (N = 778)",
          dataLabels: {
          	format: 'Ibu (std IV)'
          },
          marker: {
            radius: r_base * (1 + 778/n*r_mult)
          },
        },
        {
        	id: '4',
          name: "Ibuprofen, standard oral dose (N = 537)",
          dataLabels: {
          	format: 'Ibu (std PO)'
          },
          marker: {
          	radius: r_base * (1 + 537/n*r_mult),
          },
        }, {
          id: '5',
          name: 'Acetaminophen, oral (N = 202)',
          dataLabels: {
            format: "Acet (PO)"
          },
          marker: {
            radius: r_base * (1 + 202/n*r_mult)
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
          name: 'Ibuprofen, high oral dose (N = 30)',
          dataLabels: {
            format: "Ibu (high PO)"
          },
          marker: {
            radius: r_base * (1 + 30/n*r_mult)
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
          name: 'Indomethacin, continuous IV infusion (N = 49)',
          dataLabels: {
            format: "Indo (cont IV)"
          },
          marker: {
            radius: r_base * (1 + 49/n*r_mult)
          }
        }, {
          id: '10',
          name: 'Indomethacin, other types (N = 387)',
          dataLabels: {
            format: "Indo other"
          },
          marker: {
            radius: r_base * (1 + 387/n*r_mult)
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
          width: 2,
        }, {
        	from: '1',
          to: '4',
          width: 3,
        }, {
          from: '1',
          to:'10',
          width: 1
        }, {
          from: '2',
          to:'3',
          width: 10
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
          width: 1
        }, {
          from: '2',
          to:'10',
          width: 6
        }, {
          from: '3',
          to:'4',
          width: 3
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
          width: 1
        }, {
          from: '4',
          to:'10',
          width: 3
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
