GERBIL_PATH := $(CURDIR)/.gerbil
export GERBIL_PATH

test: run-mdp run-qlearning

run-qlearning:
	gxi frozen_lake_qlearning.ss

run-mdp:
	gxi mdp_demo.ss

clean:
	rm -rf .gerbil *~
