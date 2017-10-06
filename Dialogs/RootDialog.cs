using Microsoft.Bot.Builder.Dialogs;
using Microsoft.Bot.Connector;
using System;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace Sabbath.Bot.Dialogs
{
    [Serializable]
	public class RootDialog : IDialog<object>
	{
		public Task StartAsync(IDialogContext context)
		{
            context.Wait(MessageReceivedAsync);

			return Task.CompletedTask;
		}

        private async Task MessageReceivedAsync(IDialogContext context, IAwaitable<IMessageActivity> result)
        {
            var message = await result;

            await this.SendWelcomeMessageAsync(context);
        }

        private async Task SendWelcomeMessageAsync(IDialogContext context)
        {
            await context.PostAsync("Hi, let me tell you a joke.  Do you want to hear about chickens or lawyers?");

            context.Wait(JokeTypeSelectedAsync);
        }

        private async Task JokeTypeSelectedAsync(IDialogContext context, IAwaitable<IMessageActivity> result)
        {
            var whatUserSaid = await result;

            if (Regex.IsMatch(whatUserSaid.Text, "chicken", RegexOptions.IgnoreCase))
            {
                context.Call(new ChickenJokeDialog(), this.JokeDialogResumeAfter);
            }
            else
            {
                await context.PostAsync("Lawyer jokes it is then!");
                context.Call(new LawyerJokeDialog(), JokeDialogResumeAfter);
            }
        }

        private async Task JokeDialogResumeAfter(IDialogContext context, IAwaitable<object> result)
        {
            var returnValue = await result;
            await context.PostAsync("Thanks for joking!");
            await this.SendWelcomeMessageAsync(context);
        }
	}
}