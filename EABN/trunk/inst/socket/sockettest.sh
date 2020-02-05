#!/bin/bash
# Usage:  Have socketTest.R running, then ./sockettest.sh "message"
EABNSOCK = 3
EABNPORT = 5885

exec ${EABNSOCK}<>/dev/tcp/localhost/${EABNPORT}

printf $1 >&${EABNSOCK}
cat >&${EABNSOCK} 
