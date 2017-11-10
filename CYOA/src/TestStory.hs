module TestStory (testStory) where

import StoryData

-- | A story with some test content, to test the story execution
testStory :: Story
testStory = Story {
    storyTitle = "Test Story", 
    startPage = Page {
        pageText = [
            "You wake up.",
            "You remember your identity.",
            "You are a fish, and your name is Frederick.",
            "Precisely how you are thinking this is a great mystery, as fish are not normally sapient.",
            "Chalk that one up to the mysteries of the universe, I guess.",
            "Nonetheless, you are a sapient fish and your name is Frederick.",
            "You live on land, and for all intents and purposes your life is that of a regular human. Nobody suspects anything.",
            "You decide to use your sapience and the free will (or illusion thereof) it grants you to make a Meaningful Choice"
            ],
        pageResult = Choices {
            choiceQuestion = "Do you get out of bed?",
            choiceOptions = [
                Choice {
                    choiceLabel = "Get out of bed",
                    nextPage = Page {
                        pageText = [
                            "You get out of bed and wander into your kitchen.",
                            "As you retrieve your cereal, you hear a deafening noise.",
                            "You turn around, and the other half of your house is no longer there.",
                            "It would appear to have been replaced by a large amount of debris, and a heavy-looking shipping container.",
                            "The container's centre of mass is right over where the bed you were lying on moments prior used to be.",
                            "It seems fortuitous that you made the Meaningful Choice to get up and have breakfast, as you would certainly have been crushed had you stayed in bed.",
                            "Your auditory sensing organs (fish don't have ears, per se) are still ringing, and the whole situation seems entirely surreal."
                        ],
                        pageResult = Choices {
                            choiceQuestion = "What do you do?",
                            choiceOptions = [
                                Choice {
                                    choiceLabel = "Run out of your house screaming and crying",
                                    nextPage = Page {
                                        pageText = [
                                            "You (correctly) realise that shipping containers falling from the sky is not an ordinary occurrence.",
                                            "You then (correctly) realise that the proper response to your house being crushed by a large object is unrestrained panic.",
                                            "You attempt to bolt out of your house through the portion of your front doorway which is still intact.",
                                            "Unfortunately in your panic you do not register the steel beam sticking partway out of the doorway",
                                            "The steel is sharp and slices you open.",
                                            "The wound is horrific, and you bleed out on your front lawn.",
                                            "Your neighbours have come out to see what is going on, and they see you, fish entrails hanging out, dying on the freshly cut grass.",
                                            "It is only now that they realise you weren't just their neighbour - you were the heart and soul of the community.",
                                            "They also realise you were actually a fish this whole time, and since they aren't a wasteful lot they'd best make use of your delicious fish carcass.",
                                            "You are grilled and eaten, and the omega 3 fatty acids your flesh naturally contains provide sustenance and promote health in those who eat it.",
                                            "Thus even in death you provided a service to those who knew you best."
                                        ],
                                        pageResult = EndPoint $ Ending "You died, but you made the world a better place, and isn't that *true* immortality?"
                                    }
                                },
                                Choice {
                                    choiceLabel = "Calmly grab your cereal, because breakfast is the most important meal of the day",
                                    nextPage = Page {
                                        pageText = [
                                            "You mentally convince yourself that this is okay, and everything is going to be okay.",
                                            "After all, that half of your house was your least favourite half and had been in dire need of renovations anyway.",
                                            "You return to your pantry and grab a box of cereal.",
                                            "As you go to grab it out, another shipping container lands, this time right on top of you.",
                                            "You are crushed to death instantaneously."
                                        ],
                                        pageResult = EndPoint $ Ending "You died. Worse, your house is completely unsalvageable."
                                    }
                                }
                            ]
                        }
                    }
                },
                Choice {
                    choiceLabel = "Stay in bed",
                    nextPage = Page {
                        pageText = [
                            "You stay in bed and go back to sleep.",
                            "Unfortunately, staying in bed was a very Meaningful Choice.",
                            "A large shipping container falls from the sky and crashes through your roof, crushing you instantly."
                        ],
                        pageResult = EndPoint $ Ending "You died. At least your death was Meaningful."
                    }
                }
            ]
        } 
    }
}