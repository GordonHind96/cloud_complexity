#!/bin/bash
stack exec CLOUD-COMPLEXITY-exe worker localhost 8000 &
stack exec CLOUD-COMPLEXITY-exe worker localhost 8001 &
stack exec CLOUD-COMPLEXITY-exe worker localhost 8002 &
stack exec CLOUD-COMPLEXITY-exe worker localhost 8003 &
