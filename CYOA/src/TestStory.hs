module TestStory (testStory) where

import VariableData
import StoryData

-- | A story with some test content, to test the story execution
testStory :: Story
testStory = Story {
    storyTitle = "Test Story", 
    startPage = Page {
        pageContents = [
            pLine "A couple of quick questions about ::? 5 < 2 ? birds : llamas ?:: before we begin:",
            pVarPrompt "What is your name?" "name" StringVariable,
            pVarPrompt "What is your favourite integer?" "faveInt" IntVariable,
            pVarPrompt "What is five divided by two?" "fiveDivTwo" DoubleVariable,
            pLine "Okay, let's get on with it!",
            pLine "...",
            pLine "You wake up.",
            pLine "You remember your identity.",
            pLine "You are a fish, and your name is $name.",
            pLine "Precisely how you are thinking this is a great mystery, as fish are not normally sapient.",
            pLine "Chalk that one up to the mysteries of the universe, I guess.",
            pLine "Nonetheless, you are a sapient fish and your name is ${name}.",
            pLine "You live on land, and for all intents and purposes your life is that of a regular human. Nobody suspects anything.",
            pLine "You decide to use your sapience and the free will (or illusion thereof) it grants you to make a Meaningful Choice"
            ],
        pageResult = Choices {
            choiceQuestion = Line "Do you get out of bed?",
            choiceOptions = [
                Choice {
                    choiceLabel = Line "Get out of bed",
                    nextPage = Page {
                        pageContents = [
                            pLine "You get out of bed and wander into your kitchen.",
                            pLine "As you retrieve your cereal, you hear a deafening noise.",
                            pLine "You turn around, and the other half of your house is no longer there.",
                            pLine "It would appear to have been replaced by a large amount of debris, and a heavy-looking shipping container.",
                            pLine "The container's centre of mass is right over where the bed you were lying on moments prior used to be.",
                            pLine "It seems fortuitous that you made the Meaningful Choice to get up and have breakfast, as you would certainly have been crushed had you stayed in bed.",
                            pLine "Your auditory sensing organs (fish don't have ears, per se) are still ringing, and the whole situation seems entirely surreal."
                        ],
                        pageResult = Choices {
                            choiceQuestion = Line "What do you do?",
                            choiceOptions = [
                                Choice {
                                    choiceLabel = Line "Run out of your house screaming and crying",
                                    nextPage = Page {
                                        pageContents = [
                                            pLine "You (correctly) realise that shipping containers falling from the sky is not an ordinary occurrence.",
                                            pLine "You then (correctly) realise that the proper response to your house being crushed by a large object is unrestrained panic.",
                                            pLine "You bolt out of your house through the portion of your front doorway which is still intact.",
                                            pLine "Unfortunately in your panic you do not register the steel beam sticking partway out of the doorway",
                                            pLine "The steel is sharp and slices you open.",
                                            pLine "The wound is horrific, and you bleed out on your front lawn.",
                                            pLine "Your neighbours have come out to see what is going on, and they see you, fish entrails hanging out, dying on the freshly cut grass.",
                                            pLine "It is only now that they realise you weren't just their neighbour - you were the heart and soul of the community.",
                                            pLine "They also realise you were actually a fish this whole time, and since they aren't a wasteful lot they'd best make use of your delicious fish carcass.",
                                            pLine "You are grilled and eaten, and the omega 3 fatty acids your flesh naturally contains provide sustenance and promote health in those who eat it.",
                                            pLine "Thus even in death you provided a service to those who knew you best."
                                        ],
                                        pageResult = EndPoint $ Ending $ Line "You died, but you made the world a better place, and isn't that *true* immortality?"
                                    }
                                },
                                Choice {
                                    choiceLabel = Line "Calmly grab your cereal, because breakfast is the most important meal of the day",
                                    nextPage = Page {
                                        pageContents = [
                                            pLine "You mentally convince yourself that this is okay, and everything is going to be okay.",
                                            pLine "After all, that half of your house was your least favourite half and had been in dire need of renovations anyway.",
                                            pLine "You return to your pantry and grab a box of cereal.",
                                            pLine "As you go to grab it out, another shipping container lands, this time right on top of you.",
                                            pLine "You are crushed to death instantaneously."
                                        ],
                                        pageResult = EndPoint $ Ending $ Line "You died. Worse, your house is completely unsalvageable."
                                    }
                                }
                            ]
                        }
                    }
                },
                Choice {
                    choiceLabel = Line "Stay in bed",
                    nextPage = Page {
                        pageContents = [
                            pLine "You stay in bed and go back to sleep.",
                            pLine "Unfortunately, staying in bed was a very Meaningful Choice.",
                            pLine "A large shipping container falls from the sky and crashes through your roof, crushing you instantly."
                        ],
                        pageResult = EndPoint $ Ending $ Line "You died. At least your death was Meaningful."
                    }
                }
            ]
        } 
    }
}