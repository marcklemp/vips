# vips.el

`vips.el` is a simple but powerful Emacs interface for OpenAI's GPT API and DeepL's translation API. This tool allows you to leverage the capabilities of these APIs directly within your Emacs environment.

## Key Features

- Work directly with OpenAI's LLM models, such as GPT-4 and GPT-4 Turbo, in Emacs.
- Customize parameters for OpenAI's API, including max tokens, temperature, top-p, frequency penalty, and presence penalty to fine-tune the output.
- Translate text seamlessly using the DeepL API.
- Choose your target language for translation.

## Installation Steps

1. Download `vips.el` to your local system.
2. Add the directory containing `vips.el` to your `load-path` using `(add-to-list 'load-path "/path/to/your/directory")`.
3. Load the package with `(require 'vips)`.

## How to Use

Activate `vips-mode` in your Emacs buffer by executing `M-x vips-mode`.

Once `vips-mode` is active, you can easily interact with OpenAI's GPT models and DeepL's translation API to enhance your productivity within Emacs.

### Interacting with OpenAI's GPT Models

You can send text to OpenAI's models and receive AI-generated responses. Even without setting system messages, the GPT models will operate effectively in a standard manner.

- `C-c <left>`: Send selected text to GPT-4 and insert the response.
- `C-c <right>`: Send selected text to GPT-4 Turbo and insert the response.
- `C-c <down>`: Choose a specific model and send selected text to it.
- `C-c C-a C-c`: Mark text from the start of the buffer to the current position and run GPT-4 Turbo.
- `C-c C-a C-v`: Run GPT-4 Turbo on the entire buffer.
- `C-c C-d C-s`: Display currently selected system messages.

### Customizing AI Responses with System Messages

For more tailored interactions, you can define system messages that provide context or instructions to the AI model. These messages are divided into two categories: main system messages and add-on messages.

- **Main System Messages**: Set general instructions or roles for the AI, like "You are a helpful assistant."
- **Add-On Messages**: Specify additional guidelines, such as "Use Markdown markup."

#### Managing System Messages

- Use `M-x vips-select-main-system-message` and `M-x vips-select-addon-message` to choose main and add-on messages respectively.
- The selected messages are combined automatically and sent with your text to the AI.
- Clear selected messages with `M-x vips-clear-selected-message` if you wish to reset the AI's context.
- View the current combined message with `M-x vips-display-selected-messages`.

### Translating Text with DeepL

Translate text directly within Emacs using DeepL's powerful translation API.

- `C-c SPC`: Select output language, translate the selected text using DeepL, and append the translation.

### Configuration

Before using `vips-mode`, configure your API keys for OpenAI and DeepL in your Emacs settings.

With `vips-mode`, you have the flexibility to use GPT models in their standard configuration or customize their responses with system messages. This allows for a personalized experience whether you're drafting documents, coding, or communicating in multiple languages.

## Important Notes

To use this tool, you must have valid API keys for GPT and/or DeepL.

Add the following code to your config.el file:

```emacs-lisp
(setq openai-api-key "<your-openai-key-here>")
(setq deepl-api-key "<your-deepl-key-here>")
```

Replace <your-openai-key-here> and <your-deepl-key-here> with your actual API keys.

The `vips-languages` variable contains the list of language codes available for the translation function, initially set to include Danish ("DA") and English ("EN"). To modify this list, you can add a `setq` form to your Emacs configuration file (.emacs or init.el), like so:

```emacs-lisp
(setq vips-languages '("DA" "EN" "DE" "FR"))
```

This will add German ("DE") and French ("FR") to the list of available languages.

## Alternatives

`vips.el` is not the only Emacs client for ChatGPT. There are also: [GPTel](https://github.com/karthink/gptel), [chatgpt-shell](https://github.com/xenodium/chatgpt-shell), [org-ai](https://github.com/rksm/org-ai), [chatgpt-arcana](https://github.com/CarlQLange/chatgpt-arcana.el), [leafy-mode](https://github.com/MichaelBurge/leafy-mode), and [chat.el](https://github.com/iwahbe/chat.el).

## License

This program is distributed as free software. You can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. For more details, please refer to the [LICENSE](LICENSE) file.

## Acknowledgements

This project was inspired by `aide.el` by Junji Zhi. The POST-related code was adapted and extended from the original code in `aide.el`. We thank Junji Zhi for their contribution to the open source community. `aide.el` is licensed under the GPL license.
