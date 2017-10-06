using Microsoft.Bot.Builder.Dialogs;
using System.Threading.Tasks;
using Microsoft.Bot.Connector;
using System;

namespace Sabbath.Bot.Dialogs
{
    [Serializable]
    public class LawyerJokeDialog : IDialog<object>
    {
        public async Task StartAsync(IDialogContext context)
        {
            await context.PostAsync(@"

The attorney tells the accused, “I have some good news and some bad news.”

“What’s the bad news?” asks the accused.

“The bad news is, your blood  is all over the crime scene, and the DNA tests prove you did it.”

“What’s the good news?”

“Your cholesterol is 130.”
");

            context.Done("");
        }
    }
}