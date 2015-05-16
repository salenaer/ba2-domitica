om de simulatie te starten: 
1) op computer in racket: run simulation-majordomo.rkt
2) op raspberry pi in terminal: 
	run slip
	(load "simulation")
	(start-simulation 'ipv4 van majordomo')


om echte applicatie te starten:
1) op computer in racket: run majordomo.rkt
2) op raspberry pi in terminal: 
	run slip
	(load "steward")
	(start 'ipv4 van majordomo')