The phase1_graph.svg, phase2_graph.svg and final_graph.svg images demonstrate how I build graph in 3 steps and helps better understand inlay (nodes 2,3,4) and outlay (nodes 5,6,7)
A person that pays x amount of money less than the average (should pay x more) will be placed as a node in inlay with an arc from node 0 with value x to that node itself
A person that pays y amount of money more than the average (should receive y back) will be placed as a node in outlay with an arc from itself with value y to node 1
All the others arcs will have value of the sum of everyone's paiement
At the end, the result (who pays who and how much) is all the arc between inlay and outlay and the difference between the sum and the value of that arc