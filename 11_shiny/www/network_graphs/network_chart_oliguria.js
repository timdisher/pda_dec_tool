// Global variables
var n = 4256;
var r_base = 5;
var r_mult = 15;

Highcharts.chart("oligo_net", {
    chart: {
        type: 'networkgraph',
        height: 'auto'
    },
    title: {text: "Oliguria"},
    subtitle: {text: "<em>28 trials; 2452 infants" +
      "<br>Treatments with no data: Placebo/No treatment</em>",
      style: {"text-align": "center",
              "font-size" : '80%'},
      useHTML: false},
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
        
        nodes: [
        {
        	id: '1',
          name: "Indomethacin, IV (N = 734)",
          dataLabels: {
          	format: 'Indo (IV)'
          },
          marker:{
            radius: r_base * (1 + 734/n*r_mult)
          }
        }, 
        {
        	id: '2',
          name: "Ibuprofen, standard IV dose (N = 655)",
          dataLabels: {
          	format: 'Ibu (std IV)'
          },
          marker: {
            radius: r_base * (1 + 655/n*r_mult)
          },
        },
        {
        	id: '3',
          name: "Ibuprofen, standard oral dose (N = 367)",
          dataLabels: {
          	format: 'Ibu (std PO)'
          },
          marker: {
          	radius: r_base * (1 + 367/n*r_mult),
          },
        }, {
          id: '4',
          name: 'Acetaminophen, oral (N = 202)',
          dataLabels: {
            format: "Acet (PO)"
          },
          marker: {
            radius: r_base * (1 + 202/n*r_mult)
          }
        }, {
          id: '5',
          name: 'Ibuprofen, high IV dose (N = 35)',
          dataLabels: {
            format: "Ibu (high IV)"
          },
          marker: {
            radius: r_base * (1 + 35/n*r_mult)
          }
        }, {
          id: '6',
          name: 'Ibuprofen, high oral dose (N = 30)',
          dataLabels: {
            format: "Ibu (high PO)"
          },
          marker: {
            radius: r_base * (1 + 30/n*r_mult)
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
          name: 'Indomethacin, continuous IV infusion (N = 31)',
          dataLabels: {
            format: "Indo (cont IV)"
          },
          marker: {
            radius: r_base * (1 + 31/n*r_mult)
          }
        }
        ],
        data: [{
        	from: '1',
          to: '2',
          width: 9,
        }, {
        	from: '1',
          to: '4',
          width: 1,
        }, {
        	from: '1',
          to: '8',
          width: 6,
        }, {
          from: '2',
          to:'3',
          width: 4
        }, {
          from: '2',
          to:'5',
          width: 1
        }, {
          from: '2',
          to:'7',
          width: 1
        }, {
          from: '2',
          to:'8',
          width: 1
        },  {
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
