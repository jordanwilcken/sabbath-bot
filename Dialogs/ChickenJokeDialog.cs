using Microsoft.Bot.Builder.Dialogs;
using System.Threading.Tasks;
using Microsoft.Bot.Connector;
using System;

namespace Sabbath.Bot.Dialogs
{
    [Serializable]
    public class ChickenJokeDialog : IDialog<object>
    {
        public async Task StartAsync(IDialogContext context)
        {
            await context.PostAsync("Why did the chicken cross the road?");

            context.Wait(MessageReceivedAsync);
        }

        private async Task MessageReceivedAsync(IDialogContext context, IAwaitable<IMessageActivity> result)
        {
            var message = await result;

            string response = message.Text ?? "";
            await context.PostAsync($"{response}! That's an interesting idea.");
            await context.PostAsync("But no. The chicken crossed the road simply to get to the other side.");
            context.Done("");
        }
    }
}