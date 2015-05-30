module HaskellRoguelike.Actor where

import HaskellRoguelike.Entity

data Event = SoundEvent String | ActionEvent Action EntityID

data AIData = PlayerAIData [Event] | PassiveAIData | RandomAIData

data Actor = Actor EntityID AIData

