function dateToDM(date) {
    var d = date.getDate();
    var m = date.getMonth() + 1; //Month from 0 to 11    
    return "" + (d <= 9 ? "0" + d : d) + "/" + (m<=9 ? "0" + m : m);
}

function isArray(obj) {
    return Object.prototype.toString.call(obj) === "[object Array]";
}

function splat(obj) {
    return isArray(obj) ? obj : [obj];
}

function formatToChart(data, value, x = "date") {
    const locations = [...new Set(data.map(item => item.location))];
    let allseries = [];
    locations.forEach(function(loc) {
        let filtered = data.filter(function(obj) { return obj.location == loc});
        let series = [];        
        filtered.forEach(function(day) {
            if (day[value]) {
                if (x == "date") {
                    series.push([Date.parse(day[x]), day[value]]);
                } else {
                    series.push([day[x], day[value]]);
                }
            } 
        });        
        allseries.push({name:loc, data:series})
    });
    return(allseries);
}

function formatToColumnChart(data, category, value) {
    //const locations = [...new Set(data.map(item => item.location))];
    let out = [{data: []}];
    //let colors = palette("mpn65", data.length - 1);
    const total = Object.values(data).reduce((a, b) => +a + b[value], 0);
    for (i = 0; i < data.length; i++) {
    //data.forEach(function(cat) {
        let pct = Math.round(10000 * data[i][value] / total)/100;
        out[0].data.push({name:data[i][category], y:data[i][value], color:myColors[i], pct:pct});        
    }
    out[0].data.sort((a, b) => parseFloat(b.y) - parseFloat(a.y));
    let catOrder = [...new Set(out[0].data.map(item => item.name))];
    return {series:out, order:catOrder};
}

function summarise(data, group, value, newkey, newvalue, usechange = false) {
    sums = {};
    for (let i = 0; i < data.length; i++) {
        if(!sums[data[i][group]]) {
            sums[data[i][group]] = data[i][value];            
        } else {
            sums[data[i][group]] += data[i][value];
        }        
    }
    var element = {};
    var out = [];
    for (i = 0; i < Object.keys(sums).length; i++) {
        let newElement = Object.assign({}, element);        
        let key = Object.keys(sums)[i];
        newElement[group] = key;        
        newElement[newkey] = newvalue;        
        if (!usechange) {
            newElement[value] = sums[key];
        } else {
            newElement[value] = i > 0 ? Math.round(100 * 100 * (sums[key] - sums[Object.keys(sums)[i - 1]])/sums[Object.keys(sums)[i - 1]])/100 : 0;
        }
        out.push(newElement);
    }
    return out;
}

function alignData(data, value, newkey) {
    var tmp = [], out = [];
    for (i = 0; i < data.length; i++) {
        if (data[i][value] >= 100) {
            tmp.push(data[i]);
        }
    }
    tmp.sort((a,b) => (a.location > b.location) ? 1 : ((b.location > a.location) ? -1 : 0));
    let loc = tmp[0].location;
    out.push(tmp[0]);
    out[0][newkey] = 1;
    for (i = 1; i < tmp.length; i++) {
        if (tmp[i].location == loc) {
            out.push(tmp[i]);
            out[i][newkey] = out[i - 1][newkey] + 1;
        } else {
            out.push(tmp[i]);
            out[i][newkey]  = 1;
            loc = tmp[i].location;
        }
    }
    return out;
}

function getLastDate(data, value) {
    let lastDate = data[data.length - 1].date;
    let filtered = data.filter(function(obj) { return obj.date == lastDate});
    return filtered;
}

function daysToMs(days) {
    return days * 24 * 3600 * 1000;
}

var regioes = {"Sudeste": ["São Paulo", "Rio de Janeiro", "Minas Gerais", "Espírito Santo"],
               "Centro-Oeste": ["Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul"],
               "Nordeste": ["Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia"],
               "Sul": ["Paraná", "Santa Catarina", "Rio Grande do Sul"],
               "Norte": ["Acre", "Amazonas", "Pará", "Rondônia", "Roraima", "Amapá", "Tocantins"]}
               
var acronyms = {"Acre":"AC", "Alagoas":"AL", "Amazonas":"AM", "Bahia":"BA", "Ceará":"CE", "Distrito Federal":"DF", "Espírito Santo":"ES", "Goiás":"GO", "Amapá":"AP", "Maranhão":"MA", "Mato Grosso":"MT", "Mato Grosso do Sul":"MS", "Minas Gerais":"MG", "Paraná":"PR", "Paraíba":"PB", "Pará":"PA", "Pernambuco":"PE", "Piauí":"PI", "Rio Grande do Norte":"RN", "Rio Grande do Sul":"RS", "Rio de Janeiro":"RJ", "Rondônia":"RO", "Roraima":"RR", "Santa Catarina":"SC", "Sergipe":"SE", "São Paulo":"SP", "Tocantins":"TO"}

var nomesEstados = Object.keys(acronyms).sort();

regioes["Brasil"] = nomesEstados;

//Manually add first color to avoid the palette's red as the first and main color.
var myColors = ["#2b547e"].concat(palette("mpn65", 26).map(c => "#" + c));

var highchartsOptions = Highcharts.setOptions({
      lang: {
            loading: "Aguarde...",
            months: ["Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"],
            weekdays: ["Domingo", "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado"],
            shortMonths: ["Jan", "Feb", "Mar", "Abr", "Maio", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"],
            viewFullscreen: "Ver em tela cheia",
            exitFullscreen: "Sair da tela cheia",            
            printChart: "Imprimir",
            rangeSelectorFrom: "De",
            rangeSelectorTo: "Até",
            rangeSelectorZoom: "Periodo",
            downloadPNG: "Download imagem PNG",
            downloadJPEG: "Download imagem JPEG",
            downloadPDF: "Download documento PDF",
            downloadSVG: "Download imagem SVG",
            numericSymbols: null,
            thousandsSep: ","
        },         
        chart: {
            borderWidth: 1,
            borderColor: "#666666",
            borderRadius: 10,
            spacingBottom: 16,
            backgroundColor: "#FEFEFE",
            zoomType: "x",
            resetZoomButton: {
                position: {
                    x: -150,
                    y: 10
                },
                relativeTo: "chart",
                theme: {
                    height: 14
                }
            },
            panning: true,
            panKey: "ctrl"
        },
        subtitle: {
            text: "Fonte: Ministério da Saúde",
            align: "left",
            verticalAlign: "bottom",
            floating: true,
            style: {
                "margin-top": "-30px"
            },
            y: 28
        },
        colors: myColors
    }      
);

function createLineChart(renderTo) {
    Highcharts.chart({ 
        chart: {
            renderTo: renderTo,
            type: "line",            
            styledMode: false
        },
        data: {
            dateFormat: "YYYY-mm-dd"
        },
        xAxis: {
            type: "datetime",            
            dateTimeLabelFormats: {
                day: "%d/%m"
            },
            tickInterval: daysToMs(1),
            labels: {
                rotation: -45
            }
        },
        yAxis : {
            min: 1,
            labels: {
                format: "{value:,.0f}"
            }
        },
        legend: {            
            enabled: false
        },
        plotOptions: {
            line: {
                marker: {
                    enabled: true,
                    radius: 3
                }
            }
        },
        tooltip: {
            crosshairs: true,
            shared: true,
            dateTimeLabelFormats: {
                day: "%A, %d/%m/%Y"
            },
            outside: true,
            formatter: function (tooltip) {
                var items = this.points || splat(this),
                    series = items[0].series,
                    s;
                items.sort(function(a, b){
                    return ((a.y < b.y) ? 1 : ((a.y > b.y) ? -1 : 0));
                });                
                return tooltip.defaultFormatter.call(this, tooltip);
            }
        },
        title: {
            align: "left"
        },        
        defs: {
              custombtn: {        
                tagName: "pattern",
                id: "custom-btn-bg",              
                width: 100                
            }
        },
        exporting: {
            buttons: {
                customButton: {
                    text: "Logarítmica",
                    id: "btn",                    
                    onclick: function() {                        
                        if (this.yAxis[0].isLog) {
                            this.yAxis[0].update({
                                type: "linear"
                            });
                            this.exportSVGElements[2].attr({ text: "Logarítmica" });
                        } else {
                            this.yAxis[0].update({
                                type: "logarithmic"
                            });
                            this.exportSVGElements[2].attr({ text: "Linear" });
                        }                        
                    },
                    theme: {
                        width: 70,
                        "text-align": 'center'
                    }
                }
            }
        }
    }); 
}

function createColumnChart(renderTo, tooltipHelp = null) {
    Highcharts.chart(renderTo, {
        chart: {
            type: "column"            
        },
        plotOptions: {
            series: {
                groupPadding: 0,
                colorByPoint: false
            },            
        },
        tooltip: {
            shared: true,
            formatter: function() {
                var point = this.point,
                    series = this.series,                    
                    pct                
                pct = this.points[0].point.pct;
                name = this.points[0].point.name;
                return "<small>" + name + "</small>:<br><b>" + this.y + "</b> (" + pct + "% dos " + tooltipHelp + ")";
            }
        },
        legend: {
            align: "center",
            floating: false,
            verticalAlign: "bottom",
            layout: "horizontal",
            enabled: false,
            labelFormatter: function () {
                return acronyms[this.name];
            }
        }      
    });
}
document.addEventListener("DOMContentLoaded", function () {
    
    nomesEstados.forEach(function(e) {
        $("#sel_estado").append(new Option(e, e));
        
    });
    $("#sel_estado").val("Distrito Federal");
    
    ["Sudeste", "Centro-Oeste", "Nordeste", "Sul", "Norte"].forEach(function(e) {
        $("#sel_regiao").append(new Option(e, e));
    });
    
    
    // Brasil
    ////Total de casos no Brasil
    createLineChart("br_casos");
    $("#br_casos").highcharts().addSeries({            
        name: "Brasil",
        data: formatToChart(summarise(estados, "date", "total_cases", "location", "Brasil"), "total_cases")[0].data       
    }, false);
    $("#br_casos").highcharts().update({
        yAxis: {
            title: {
                text: "Casos Confirmados"
            }
        },
        title: {
            text: "Casos confirmados"
        }
    });
    
    ////Total de óbitos no Brasil
    createLineChart("br_obitos");
    $("#br_obitos").highcharts().addSeries({               
        name: "Brasil",
        data: formatToChart(summarise(estados, "date", "total_deaths", "location", "Brasil"), "total_deaths")[0].data       
    }, false);
    $("#br_obitos").highcharts().update({
        yAxis: {
            title: {
                text: "Óbitos"
            }
        },
        title: {
            text: "Óbitos"
        }
    });
    
    ////Novos casos no Brasil
    createLineChart("br_novoscasos");
    $("#br_novoscasos").highcharts().addSeries({            
        name: "Novos",
        yAxis: 0,
        data: formatToChart(summarise(estados, "date", "new_cases", "location", "Novos"), "new_cases")[0].data       
    }, false);
    $("#br_novoscasos").highcharts().update({
        yAxis: {
            title: {
                text: "Novos casos",
                style: {
                    color: myColors[0]
                }
            },
            labels: {
                style: {
                    color: myColors[0]
                }
            }
        },
        title: {
            text: "Novos casos"
        }
    }, false);
    $("#br_novoscasos").highcharts().addAxis({        
        title: {
            text: "Variação em relação ao dia anterior",
            style: {
                color: myColors[1]
            }
        },
        min: 0,
        max: 100,
        opposite: true,
        labels: {
            format: "{value}%",
            style: {
                color: myColors[1]
            }
        }
    }, false);
    $("#br_novoscasos").highcharts().addSeries({            
        name: "Aumento",
        yAxis: 1,
        data: formatToChart(summarise(estados, "date", "total_cases", "location", "Aumento", true), "total_cases")[0].data,
        tooltip: {
            valueSuffix: "%"
        }
    }, true);
    
    ////Novos óbitos no Brasil
    createLineChart("br_novosobitos");
    $("#br_novosobitos").highcharts().addSeries({               
        name: "Brasil",
        data: formatToChart(summarise(estados, "date", "new_deaths", "location", "Brasil"), "new_deaths")[0].data       
    }, false);
    $("#br_novosobitos").highcharts().update({
        yAxis: {
            title: {
                text: "Novos óbitos",
                style: {
                    color: myColors[0]
                }
            },
            labels: {
                style: {
                    color: myColors[0]
                }
            }
        },
        title: {
            text: "Novos óbitos",
        }
    });
    $("#br_novosobitos").highcharts().addAxis({        
        title: {
            text: "Variação em relação ao dia anterior",
            style: {
                color: myColors[1]
            }
        },
        min: 0,
        max: 100,
        opposite: true,
        labels: {
            format: "{value}%",
            style: {
                color: myColors[1]
            }
        }
    }, false);
    $("#br_novosobitos").highcharts().addSeries({            
        name: "Aumento",
        yAxis: 1,
        data: formatToChart(summarise(estados, "date", "total_deaths", "location", "Aumento", true), "total_deaths")[0].data,
        tooltip: {
            valueSuffix: "%"
        }
    }, true);
    
    let estados_dist_casos = formatToColumnChart(getLastDate(estados, "total_cases"), "location", "total_cases");
    //Object must be cloned so the calculation per hab doesn"t change main object
    let estados_dist_casos_hab = jQuery.extend(true, {}, estados_dist_casos);    
    for (i = 0; i < estados_dist_casos_hab.series[0].data.length; i++) {
        var point = estados_dist_casos_hab.series[0].data[i]
        point.y =  Math.round(100 * 100000 * point.y/populacao[0][point.name])/100;
    }
    estados_dist_casos_hab.series[0].data.sort((a, b) => parseFloat(b.y) - parseFloat(a.y));
    estados_dist_casos_hab.order = [...new Set(estados_dist_casos_hab.series[0].data.map(item => item.name))];
    
    createColumnChart("estados_dist", "casos");
    $("#estados_dist").highcharts().addSeries({
        //DON"T USE OBJECT NAME HERE, or the object will be changed by ref when data mode is switched
        data: formatToColumnChart(getLastDate(estados, "total_cases"), "location", "total_cases").series[0].data        
        //data: formatToChart(summarise(estados, "date", "total_deaths", "location", "Brasil"), "total_deaths")[0].data       
    }, false);
    $("#estados_dist").highcharts().update({
        title: {
            text: "Distribuição de casos por estado"
        },  
        xAxis: {        
            crosshair: true,
            categories: estados_dist_casos.order
        },        
        yAxis: {        
            title: {
                text: "Casos Confirmados"
            }
        },
        exporting: {
            buttons: {
                customButton: {
                    text: "Por Hab.",       
                    onclick: function() {
                        if (this.exportSVGElements[2].text.textStr == "Por Hab.") {                            
                            this.series[0].update({data: estados_dist_casos_hab.series[0].data}, false);
                            this.xAxis[0].setCategories(estados_dist_casos_hab.order, false);
                            this.yAxis[0].setTitle({text: "Casos por 100 mil habitantes"});
                            this.exportSVGElements[2].attr({ text: "Total" });                            
                        } else {
                            this.series[0].update({data: estados_dist_casos.series[0].data}, false);           
                            this.xAxis[0].setCategories(estados_dist_casos.order, false);                            
                            this.yAxis[0].setTitle({text: "Casos Confirmados"});
                            this.exportSVGElements[2].attr({ text: "Por Hab." });                            
                        }
                    },
                    theme: {
                        width: 60,
                        "text-align": 'center'
                    }
                }
            }
        }  
    });
    
    let estados_dist_obitos = formatToColumnChart(getLastDate(estados, "total_deaths"), "location", "total_deaths");
    //Object must be cloned so the calculation per hab doesn"t change main object
    let estados_dist_obitos_hab = jQuery.extend(true, {}, estados_dist_obitos);    
    for (i = 0; i < estados_dist_obitos_hab.series[0].data.length; i++) {
        var point = estados_dist_obitos_hab.series[0].data[i]
        point.y =  Math.round(100 * 100000 * point.y/populacao[0][point.name])/100;
    }
    estados_dist_obitos_hab.series[0].data.sort((a, b) => parseFloat(b.y) - parseFloat(a.y));
    estados_dist_obitos_hab.order = [...new Set(estados_dist_obitos_hab.series[0].data.map(item => item.name))];
    
    createColumnChart("estados_dist_obitos", "óbitos");
    $("#estados_dist_obitos").highcharts().addSeries({
        //DON"T USE OBJECT NAME HERE, or the object will be changed by ref when data mode is switched
        data: formatToColumnChart(getLastDate(estados, "total_deaths"), "location", "total_deaths").series[0].data        
    }, false);
    $("#estados_dist_obitos").highcharts().update({
        title: {
            text: "Distribuição de óbitos por estado"
        },  
        xAxis: {        
            crosshair: true,
            categories: estados_dist_obitos.order
        },        
        yAxis: {        
            title: {
                text: "Óbitos"
            }
        },
        exporting: {
            buttons: {
                customButton: {
                    text: "Por Hab.",                
                    onclick: function() {
                        if (this.exportSVGElements[2].text.textStr == "Por Hab.") {                            
                            this.series[0].update({data: estados_dist_obitos_hab.series[0].data}, false);
                            this.xAxis[0].setCategories(estados_dist_obitos_hab.order, false);
                            this.yAxis[0].setTitle({text: "Óbitos por 100 mil habitantes"});
                            this.exportSVGElements[2].attr({ text: "Total" });                            
                        } else {
                            this.series[0].update({data: estados_dist_obitos.series[0].data}, false);           
                            this.xAxis[0].setCategories(estados_dist_obitos.order, false);                            
                            this.yAxis[0].setTitle({text: "Óbitos"});
                            this.exportSVGElements[2].attr({ text: "Por Hab." });                            
                        }
                    },
                    theme: {
                        width: 60,
                        "text-align": 'center'
                    }
                }
            }
        }
    });
    
    //// Único estado com seleção    
    $("#sel_estado").change(function(){
        let chartCasos =  $("#estado_casos").highcharts();        
        chartCasos.series[0].update({            
            name: $("#sel_estado").val(),
            data: formatToChart(estados.filter(function(obj) { return obj.location == $("#sel_estado").val()}), "total_cases")[0].data,            
        }, false);
        chartCasos.setTitle({text: "Casos confirmados - " + $("#sel_estado").val()});
        chartCasos.redraw();
        
        let chartObitos =  $("#estado_obitos").highcharts();        
        chartObitos.series[0].update({            
            name: $("#sel_estado").val(),
            data: formatToChart(estados.filter(function(obj) { return obj.location == $("#sel_estado").val()}), "total_deaths")[0].data,            
        }, false);
        chartObitos.setTitle({text: "Óbitos - " + $("#sel_estado").val()});
        chartObitos.redraw();
    });
    
    createLineChart("estado_casos");
    $("#estado_casos").highcharts().addSeries({               
        name: $("#sel_estado").val(),
        data: formatToChart(estados.filter(function(obj) { return obj.location == $("#sel_estado").val()}), "total_cases")[0].data     
    }, false);
    $("#estado_casos").highcharts().update({        
        yAxis: {
            title: {
                text: "Casos Confirmados"
            }
        },
        title: {
            text: "Casos Confirmados - " + $("#sel_estado").val()
        }
    });
    
    createLineChart("estado_obitos");
    $("#estado_obitos").highcharts().addSeries({               
        name: $("#sel_estado").val(),
        data: formatToChart(estados.filter(function(obj) { return obj.location == $("#sel_estado").val()}), "total_deaths")[0].data       
    }, false);
    $("#estado_obitos").highcharts().update({         
        yAxis: {
            title: {
                text: "Óbitos"
            }
        },
        title: {
            text: "Óbitos - " + $("#sel_estado").val()
        }
    });
    
    //Comparações entre estados
    
    ////Todos os estados - Casos Confirmados
    createLineChart("estados_casos");    
    
    let dataCasos  = formatToChart(estados, "total_cases");
    let chartCasos = $("#estados_casos").highcharts();
    while (chartCasos.series.length > 0) {
        chartCasos.series[0].remove(false);
    }
    for (i = 0; i < dataCasos.length; i++) {
        chartCasos.addSeries({               
            name: dataCasos[i].name,
            data: dataCasos[i].data       
        }, false);
    }
    chartCasos.update({            
        yAxis: {
            title: {
                text: "Casos Confirmados"
            }
        },
        title: {
            text: "Casos - todos estados"
        },
        legend: {
            align: "center",
            floating: false,
            verticalAlign: "bottom",
            layout: "horizontal",
            enabled: true,
            labelFormatter: function () {
                return acronyms[this.name];
            }
        }
    });
    
    ////Todos os estados - Óbitos
    createLineChart("estados_obitos");
    let dataObitos  = formatToChart(estados, "total_deaths");
    let chartObitos = $("#estados_obitos").highcharts();
    while (chartObitos.series.length > 0) {
        chartObitos.series[0].remove(false);
    }
    for (i = 0; i < dataObitos.length; i++) {
        chartObitos.addSeries({               
            name: dataObitos[i].name,
            data: dataObitos[i].data       
        }, false);
    }
    chartObitos.update({            
        yAxis: {
            title: {
                text: "Óbitos"
            }
        },
        title: {
            text: "Óbitos - todos estados"
        },
        legend: {
            align: "center",
            floating: false,
            verticalAlign: "bottom",
            layout: "horizontal",
            enabled: true,
            labelFormatter: function () {
                return acronyms[this.name];
            }
        }
    });
    
    
    ////Única região com seleção
    createLineChart("regiao_casos");
    createLineChart("regiao_obitos");    
    $("#sel_regiao").change(function(){
        let dataCasos  = formatToChart(estados.filter(function(obj) { return regioes[$("#sel_regiao").val()].includes(obj.location)}), "total_cases");
        let chartCasos = $("#regiao_casos").highcharts();
        while (chartCasos.series.length > 0) {
            chartCasos.series[0].remove(false);
        }
        for (i = 0; i < dataCasos.length; i++) {
            chartCasos.addSeries({               
                name: dataCasos[i].name,
                data: dataCasos[i].data       
            }, false);
        }
        chartCasos.update({            
            yAxis: {
                title: {
                    text: "Casos Confirmados"
                }
            },
            title: {
                text: "Casos - " + $("#sel_regiao").val()
            },
            legend: {
                align: "center",
                floating: false,
                verticalAlign: "bottom",
                layout: "horizontal",
                enabled: true,
                labelFormatter: function () {
                    return acronyms[this.name];
                }
            }
        });
        //Obitos
        let dataObitos  = formatToChart(estados.filter(function(obj) { return regioes[$("#sel_regiao").val()].includes(obj.location)}), "total_deaths");
        let chartObitos = $("#regiao_obitos").highcharts();
        while (chartObitos.series.length > 0) {
            chartObitos.series[0].remove(false);
        }
        for (i = 0; i < dataObitos.length; i++) {
            chartObitos.addSeries({               
                name: dataObitos[i].name,
                data: dataObitos[i].data       
            }, false);
        }
        chartObitos.update({            
            yAxis: {
                title: {
                    text: "Óbitos"
                }
            },
            title: {
                text: "Óbitos - " + $("#sel_regiao").val()
            },
            legend: {
                align: "center",
                floating: false,
                verticalAlign: "bottom",
                layout: "horizontal",
                enabled: true,
                labelFormatter: function () {
                    return acronyms[this.name];
                }
            }
        });
    });    
    $("#sel_regiao").change();    
    
    // Regiões
    // for (var reg in regioes) {
        // let regiao = estados.filter(function(obj) { return regioes[reg].includes(obj.location)})
        // let formattedData = formatToChart(regiao, "total_cases");
        // createLineChart("reg_" + reg);
        // for (i = 0; i < formattedData.length; i++) {
            // $("#reg_" + reg).highcharts().addSeries({               
                // name: formattedData[i].name,
                // data: formattedData[i].data       
            // }, false);
        // }
        // $("#reg_" + reg).highcharts().update({            
            // yAxis: {
                // title: {
                    // text: "Casos Confirmados"
                // }
            // },
            // title: {
                // text: "Casos - " + reg
            // },
            // legend: {
                // align: "center",
                // floating: false,
                // verticalAlign: "bottom",
                // layout: "horizontal",
                // enabled: reg != "Brasil",
                // labelFormatter: function () {
                    // return acronyms[this.name];
                // }
            // }
        // });
    // }
    
    ////Casos confirmados após o 100 caso
    let formattedData = formatToChart(alignData(estados, "total_cases", "day"), "total_cases", "day");
    createLineChart("estados_comp");
    for (i = 0; i < formattedData.length; i++) {
        $("#estados_comp").highcharts().addSeries({               
            name: formattedData[i].name,
            data: formattedData[i].data
        }, false);
    }
    $("#estados_comp").highcharts().update({  
        yAxis: {
            type: "logarithmic",
            min: 100,
            title: {
                text: "Casos Confirmados"
            }
        },
        xAxis: {
            type: "linear",
            tickInterval: 1,
            labels: {
                rotation: 0
            },
            title: {
                text: "Dia"
            }
        },
        title: {
            text: "Casos Confirmados após o 100º caso"
        },
        legend: {
            align: "center",
            floating: false,
            verticalAlign: "bottom",
            layout: "horizontal",
            enabled: true,            
            labelFormatter: function () {
                return acronyms[this.name];
            }
        },
        exporting: {
            buttons: {
                customButton: {
                    text: "Linear"
                }
            }
        }            
    });
    //Hide loading div after all charts are loaded
    $('#loading, #wall').hide();
});
