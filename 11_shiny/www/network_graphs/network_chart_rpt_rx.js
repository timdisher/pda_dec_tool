// Global variables
var n = 4256;
var r_base = 5;
var r_mult = 15; 

Highcharts.chart("rpt_rx_net", {
    chart: {
        type: 'networkgraph',
        height: 'auto'
    },
    title: {text: "Need for repeat pharmacotherapy"},
    subtitle: {text: "<em>33 trials; 2322 infants" +
      "<br>Treatments with no data: Ibuprofen (continuous IV)</em>",
      style: {"text-align": "center",
              "font-size" : '80%'}
      
    },
    
    plotOptions: {

        networkgraph: {
            layoutAlgorithm: {
            		linkLength: 100,
                enableSimulation: true
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
          name: "Placebo or no treatment (N = 121)",
          dataLabels: {
          	format: 'PBO'
          },
          marker:{
            radius: r_base * (1 + 121/n*r_mult)
          },
        },
        
        {
        	id: '2',
          name: "Indomethacin, IV (N = 601)",
          dataLabels: {
          	format: 'Indo (IV)'
          },
          marker:{
            radius: r_base * (1 + 601/n*r_mult)
          }
        }, 
        {
        	id: '3',
          name: "Ibuprofen, standard IV dose (N = 534)",
          dataLabels: {
          	format: 'Ibu (std IV)'
          },
          marker: {
            radius: r_base * (1 + 534/n*r_mult)
          },
        },
        {
        	id: '4',
          name: "Ibuprofen, standard oral dose (N = 395)",
          dataLabels: {
          	format: 'Ibu (std PO)'
          },
          marker: {
          	radius: r_base * (1 + 395/n*r_mult),
          },
        }, {
          id: '5',
          name: 'Acetaminophen, oral (N = 187)',
          dataLabels: {
            format: "Acet (PO)"
          },
          marker: {
            radius: r_base * (1 + 187/n*r_mult)
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
          name: 'Ibuprofen, high oral dose (N = 92)',
          dataLabels: {
            format: "Ibu (high PO)"
          },
          marker: {
            radius: r_base * (1 + 92/n*r_mult)
          }
        },  {
          id: '8',
          name: 'Indomethacin, continuous IV infusion (N = 31)',
          dataLabels: {
            format: "Indo (cont IV)"
          },
          marker: {
            radius: r_base * (1 + 31/n*r_mult)
          }
        }, {
          id: '9',
          name: 'Indomethacin, other types (N = 326)',
          dataLabels: {
            format: "Indo other"
          },
          marker: {
            radius: r_base * (1 + 326/n*r_mult)
          }
        }
        ],
        data: [{
        	from: '1',
          to: '2',
          width: 3,
        }, {
        	from: '1',
          to: '3',
          width: 1,
        },  {
          from: '1',
          to:'9',
          width: 2
        }, {
          from: '2',
          to:'3',
          width: 7
        }, {
          from: '2',
          to:'4',
          width: 3
        }, {
          from: '2',
          to:'9',
          width: 5
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
          width: 2
        }, {
          from: '4',
          to:'7',
          width: 1
        }, {
          from: '4',
          to:'9',
          width: 3
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
