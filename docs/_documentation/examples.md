---
layout: default
title: Examples
nav_order: 4
description: ""
permalink: /examples
---

# Examples
{: .no_toc }

Below is an interactive visualization of a two-dimensional toy dataset which was generated with the AHB R Package. Click on a treated unit shown in orange to see a representation of its matched group, which will be depicted as a red box surrounding all of the units in the group. Once selected, the marker for a treated unit will become larger. Click on the marker again to hide the matched group.

<div class="graph" id="tester" style="width:600px;height:400px;"></div>

<script>
	TESTER = document.getElementById('tester');
	var treated = {
		x: [-0.9660798,  3.2802848,  2.3606708, -4.2075829, 3.9428443,  2.2230314, -1.0307580, -1.1262030],
		y: [2.20054788, -2.08015919, -1.68417799, -0.35666177,  0.21815098,  0.14764699,  4.90472471, 0.52658809],
		type: 'scatter',
		name: 'treated',
		mode: 'markers',
		marker: {
			color: 'rgb(0.8500, 0.3250, 0.0980)',
			size: [10, 10, 10, 10, 10, 10, 10, 10]
		}
	};
	var control = {
		x: [-4.9464965, -3.0296428,  2.9002235,  0.8294913,  3.2769443,  2.9315043,  3.5832106,  -3.6877658, -1.7058992,  -1.1650632,   2.6402475, -3.6047390],
		y: [0.17233325, -0.18061857, -0.64532448, -3.32051974, -3.13171898,  0.03618141,  2.40518822,  4.81815310,  4.97818361, -1.92380463, -2.49514714, -0.06110301],
		type: 'scatter',
		name: 'control',
		mode: 'markers',
  		marker: {
			  color: 'rgb(0, 0.4470, 0.7410)',
			  size: [10, 10, 10, 10, 10, 10, 10, 10]
		  }
	};
	var data = [treated, control];
	var layout = {
		title: {
			text:"Interactive Hyper-box Example<br><span style='font-size: 14; font-weight: 300'>Click on a treated unit to toggle its hyperbox!</span>",
			font: {
				size: 24
			}
		},
		xaxis: {
			title: {
				text: 'x1',
				font: {
					size: 18
				}
			},
			zeroline: false
		},
		yaxis: {
			title: {
				text: 'x2',
				font: {
					size: 18
				}
			},
			zeroline: false
		},
		hovermode: 'closest',
		legend: {
			itemsizing: 'constant'
		},
		shapes: [
			{
				type: 'rect',
				visible: false,
				x0: -0.9660798,
				y0: -1.9237946,
				x1: 3.9428543,
				y1: 2.405198,
				line: {
					color: 'rgb(179, 0, 0)'
				}
			},
			{
				type: 'rect',
				visible: false,
				x0: 2.3606808,
				y0: -3.1317090,
				x1: 3.5832006,
				y1: -2.080159,
				line: {
					color: 'rgb(179, 0, 0)'
				}
			},
			{
				type: 'rect',
				visible: false,
				x0: -1.0307480,
				y0: -1.6841780,
				x1: 3.9428543,
				y1: 4.818163,
				line: {
					color: 'rgb(179, 0, 0)'
				}
			},
			{
				type: 'rect',
				visible: false,
				x0: -4.9465065,
				y0: -0.6453145,
				x1: -1.0307680,
				y1: 4.818143,
				line: {
					color: 'rgb(179, 0, 0)'
				}
			},
			{
				type: 'rect',
				visible: false,
				x0: -1.0307480,
				y0: -1.9237946,
				x1: 3.9428443,
				y1: 4.818143,
				line: {
					color: 'rgb(179, 0, 0)'
				}
			},
			{
				type: 'rect',
				visible: false,
				x0: -1.0307480,
				y0: -1.9237946,
				x1: 3.9428543,
				y1: 4.818143,
				line: {
					color: 'rgb(179, 0, 0)'
				}
			},
			{
				type: 'rect',
				visible: false,
				x0: -3.6877558,
				y0: -0.3566518,
				x1: -0.9660898,
				y1: 4.978194,
				line: {
					color: 'rgb(179, 0, 0)'
				}
			},
			{
				type: 'rect',
				visible: false,
				x0: -4.9465065,
				y0: -0.3566718,
				x1: -1.0307680,
				y1: 4.818143,
				line: {
					color: 'rgb(179, 0, 0)'
				}
			}
		]
	};
	var eye_solid = {
		'width': 600,
		'height': 550,
		'path': 'M572.52 241.4C518.29 135.59 410.93 64 288 64S57.68 135.64 3.48 241.41a32.35 32.35 0 0 0 0 29.19C57.71 376.41 165.07 448 288 448s230.32-71.64 284.52-177.41a32.35 32.35 0 0 0 0-29.19zM288 400a144 144 0 1 1 144-144 143.93 143.93 0 0 1-144 144zm0-240a95.31 95.31 0 0 0-25.31 3.79 47.85 47.85 0 0 1-66.9 66.9A95.78 95.78 0 1 0 288 160z'
	};
	var eye_slash_solid = {
		'width': 600,
		'height': 550,
		'path': 'M320 400c-75.85 0-137.25-58.71-142.9-133.11L72.2 185.82c-13.79 17.3-26.48 35.59-36.72 55.59a32.35 32.35 0 0 0 0 29.19C89.71 376.41 197.07 448 320 448c26.91 0 52.87-4 77.89-10.46L346 397.39a144.13 144.13 0 0 1-26 2.61zm313.82 58.1l-110.55-85.44a331.25 331.25 0 0 0 81.25-102.07 32.35 32.35 0 0 0 0-29.19C550.29 135.59 442.93 64 320 64a308.15 308.15 0 0 0-147.32 37.7L45.46 3.37A16 16 0 0 0 23 6.18L3.37 31.45A16 16 0 0 0 6.18 53.9l588.36 454.73a16 16 0 0 0 22.46-2.81l19.64-25.27a16 16 0 0 0-2.82-22.45zm-183.72-142l-39.3-30.38A94.75 94.75 0 0 0 416 256a94.76 94.76 0 0 0-121.31-92.21A47.65 47.65 0 0 1 304 192a46.64 46.64 0 0 1-1.54 10l-73.61-56.89A142.31 142.31 0 0 1 320 112a143.92 143.92 0 0 1 144 144c0 21.63-5.29 41.79-13.9 60.11z'
	};
	var config = {
		modeBarButtonsToRemove: ['zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'toggleSpikelines', 'hoverCompareCartesian', 'hoverClosestCartesian'],
		modeBarButtonsToAdd: [
			{
				name: 'Show all hyperboxes',
				icon: eye_solid,
				click: function(gd) {
					const update = {};
					for (let i = 0; i < layout.shapes.length; i++) {
						update['shapes[' + i + '].visible'] = true;
						treated.marker.size[i] = 15;
					}
					Plotly.relayout(TESTER, update);
					Plotly.restyle(TESTER, treated.marker);
				}
			},
			{
				name: 'Hide all hyperboxes',
				icon: eye_slash_solid,
				click: function(gd) {
					const update = {};
					for (let i = 0; i < layout.shapes.length; i++) {
						update['shapes[' + i + '].visible'] = false;
						treated.marker.size[i] = 10;
					}
					Plotly.relayout(TESTER, update);
					Plotly.restyle(TESTER, treated.marker);
				}
			}
		],
		displaylogo: false
	};
	Plotly.newPlot(TESTER, data, layout, config);

	TESTER.on('plotly_click', function(array) {
		if (array.points[0].data.name === 'treated') {
			const update = {};
			var index = array.points[0].pointIndex;
			var key = 'shapes[' + index + '].visible';
			update[key] = !layout.shapes[index].visible;
			Plotly.relayout(TESTER, update);

			treated.marker.size[index] = treated.marker.size[index] === 10 ? 15 : 10;
			Plotly.restyle(TESTER, treated.marker);
		}
	});
</script>