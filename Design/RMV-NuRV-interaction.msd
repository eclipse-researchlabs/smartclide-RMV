#//# --------------------------------------------------------------------------------------
#//# Created using Sequence Diagram for Mac
#//# https://www.macsequencediagram.com
#//# https://itunes.apple.com/gb/app/sequence-diagram/id1195426709?mt=12
#//# --------------------------------------------------------------------------------------
title "RMV-to-NuRV component interaction - v3"
# v3	complete detailed sequence
# v2	completed monitor creation
# v1	initial version

box "Runtime Monitoring & Verification" #00ff00 .1
participant "Monitor\nCreation" as MC
participant "Monitoring\nFramework" as MF
participant "Create\nModel" as CM
participant "Create\nProp Spec" as CPS
participant "Monitor\nLibrary" as ML
participant "NuRV\nInterface" as NuRVi
participant "NuRV" as NuRV
participant "Monitor 𝓜\nNuRV Server" as Mserv
participant "tnameserv" as TNS
participant "Notification\nAgent" as NA
participant "Monitor Event\nProcessing" as MEP
end box # Runtime Monitoring & Verification

box "Monitor Synthesis" #ff0000 .1
end box # Monitor Synth

box "SmartCLIDE Environment" #ff0000 .1
participant "Monitor\nSensor 𝓜𝑺" as MS
participant "Service\n𝑺" as S
participant "Execution\nControl" as EC
participant "Service\nDeployment" as SD
participant "Service\nCreation" as SC
participant "SmartCLIDE\nIDE/Control" as SCC
participant "Context\nHandling" as CXH
end box # Execution

activate MC
activate MF
activate CM
activate CPS
activate ML
activate NuRVi
activate NA
activate SCC
activate SC
activate SD
activate EC
activate S
activate MS
activate CXH
activate tns
activate NuRV
activate Mserv
activate TNS
activate MEP

region [INITIALIZATION]
MF->TNS: start tnameserv
TNS->MF: Interoperable Object Reference (IOR)
MF->ML: save nameservice IOR
end # region

deactivate MF
deactivate ML

activate MF
activate ML

region [SERVICE and MONITOR CREATION/DEPLOYMENT]
SCC->SC: Create Service
SC->SC: Marshall Service Spec 𝑺𝑺
SC->SC: service_spec2service(𝑺𝑺,𝑺)
SC->MC: Request Monitor Creation for Service 𝑺 (𝑺𝑺)
#MC<-->SC: Get Service Spec 𝑺𝑺 for 𝑺
MC->CM: Create Model from Service Spec(𝑺𝑺)
CM->CM: Service Spec to Model (𝑺𝑺,𝑲)
#CM->ML: Store Model 𝑲(𝑺𝑺)
CM-->MC: Model created: 𝑲(𝑺𝑺)
MC->CPS: Create Prop Specs from Service Spec(𝑺)
CPS->CPS: Service Spec to Props (𝑺𝑺,𝞿(𝑺𝑺))
#CPS->ML: Store Prop Specs 𝞿(𝑺𝑺)
CPS-->MC: Prop Specs created 𝞿(𝑺𝑺)
MC->MC: Create NuRV\nScript 𝓝,\nUnique 𝓜id
MC->ML: Store Provisional Monitor info: 𝓜id, 𝑲(𝑺𝑺), 𝞿(𝑺𝑺), 𝓝(𝑺𝑺), 𝓜(𝑲,𝞿,𝓝) 
MC->NuRVi: Trial Synthesis of Monitor for 𝑺𝑺
NuRVi<->ML: get (𝑲,𝞿,𝓝)(𝑺𝑺)
NuRVi->NuRV: Synthesize 𝓜(𝑲,𝞿,𝓝)
NuRV-->NuRVi: Monitor synthesis result 𝓜
NuRVi-->MC: Monitor synthesis result 𝓜
MC->MC: Qualify\nMonitor 𝓜
MC->MC: Create\n𝓜𝑺 config
MC->ML: Store Monitor info: 𝓜id, 𝑲(𝑺𝑺), 𝞿(𝑺𝑺), 𝓝(𝑺𝑺), 𝓜(𝑲,𝞿,𝓝), 𝓜𝑺 config

MC-->SC: Return 𝓜𝑺 Module
SC->SC: Incorporate 𝓜𝑺 into 𝑺
SC-->SCC: Service created
SCC->SD: Deploy 𝑺+𝓜𝑺
SD-->SCC: Service 𝑺+𝓜𝑺 deployed
end # region

deactivate NuRV
deactivate NuRVi
deactivate ML
deactivate SCC

activate NuRV
activate NuRVi
activate ML
activate SCC

region [MONITORING SERVICE REQUEST]
CXH->MF: Query available monitors
MF-->CXH: List of available Monitors <<𝓜a>>
CXH<->MF: Subscribe to list of Monitors <<𝓜s>>
end # region

deactivate CXH
deactivate MF
activate CXH
activate MF

region [SERVICE EXECUTION]
SCC->EC: Run Service 𝑺+𝓜𝑺
EC->S: execute 𝑺+𝓜𝑺
S->MS: 𝓜𝑺 Startup
MS->MEP: Start monitor 𝓜id

MEP->NuRVi: Start Monitor 𝓜id
NuRVi<-->ML: Get 𝓜 info
NuRVi<-->ML: Create NuRV\nsession for 𝓜
NuRVi->NuRV: start NuRV instance
NuRVi->NuRV: build_monitor 𝓜(𝑲,𝞿,𝓝)
NuRVi->NuRV: monitor_server(IOR, 𝓜id)
NuRV<-->Mserv: start 𝓜 server
Mserv<-->TNS: Register 𝓜id
NuRV-->NuRVi: NuRV server\nstarted
NuRVi-->MEP: 𝓜 running

deactivate NuRV
activate NuRV

MEP-->MS: MEP ready for 𝓜𝑺 events
MS-->S: 𝓜𝑺 initialized


S->MS: 𝓜𝑺 Step 𝓥
MS->MS: evaluate 𝓐(𝓥)
MS->MEP: heartbeat 𝓜id, 𝓥, 𝓐

MEP->NuRVi: heartbeat 𝓜id, 𝓐
NuRVi->Mserv: heartbeat 𝓜id, 𝓐
Mserv-->NuRVi: NuRV 𝓜 verdict: 𝓑4 [⟙/⟘/?/!]
NuRVi-->MEP: NuRV 𝓜 verdict: 𝓑4 [⟙/⟘/?/!]

MEP->NA: [If monitor\nnotifications] 
NA->CXH: Monitor Report: 𝓥, 𝓐(𝓥), NuRV Verdict 𝓑4 [⟙/⟘/?/!]

alt [NuRV 𝓜 Verdict is ⟙/?]
MEP->MEP: do nothing
MEP-->MS: return from heartbeat Recover=F

else [NuRV 𝓜 Verdict is ⟘/!]
MEP-->MS: return from heartbeat Recover=T
MS->S: Recovery upcall
S->S: Recovery\naction
MEP->NA: [If exception\nnotifications]
NA->EC: exception E wrt 𝑺
EC->S: 𝑺 forced termination
#S->MS: Shutdown 𝓜𝑺
#MS->MEP: 𝓜C Shutting down
#MEP->NuRVi: Close NuRV session for 𝓜
#NuRVi->Mserv: Terminate 𝓜 server
end

S->S: 𝑺 completes
S->MS: 𝓜𝑺 Shutdown
MS->MEP: Stop Monitor 𝓜id
MEP->NuRVi: Stop Monitor 𝓜id
NuRVi<-->ML: Remove NuRV\nsession for 𝓜
NuRVi->Mserv: Stop 𝓜 server
S->EC: 𝑺 normal completion
EC->SCC: Service 𝑺+𝓜𝑺 completed
end # region

deactivate CXH
deactivate SCC
deactivate SR
deactivate RSV
deactivate SC
deactivate SD
deactivate MOM
deactivate MC
deactivate MF
deactivate CM
deactivate CPS
deactivate ML
deactivate NuRV
deactivate NuRVi
deactivate Mserv
deactivate TNS
deactivate AA
deactivate LA
deactivate NA
deactivate EC
deactivate S
deactivate MEP
deactivate MS
