# vips.el

`vips.el` is a simple but powerful Emacs interface for
- OpenAI's GPT and text-to-speech APIs
- Locally hosted language model APIs using LM Studio or other APIs that behaves like OpenAI's API
- DeepL's translation API.

This tool allows you to leverage the capabilities of these APIs directly within your Emacs environment.

## Key Features

- Work directly with OpenAI's LLMs, such as GPT-4 and GPT-4 Turbo, in Emacs.
- Do the same with locally hosted LLMs.
- Directly interact with OpenAI's text-to-speech model, enabling the output to be played within Emacs (if your version supports it) or through your operating system's default player.
- The synthesized speech can be saved as an mp3 file for future reference and use.
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

#### GPT Keybindings

- `C-c <left>`: Send selected text to GPT-4 and insert the response.
- `C-c <right>`: Send selected text to GPT-4 Turbo and insert the response.
- `C-c <down>`: Choose a specific model and send selected text to it.
- `C-c C-a C-c`: Mark text from the start of the buffer to the current position and run GPT-4 Turbo.
- `C-c C-a C-v`: Run GPT-4 Turbo on the entire buffer.

#### Customizing AI Responses with System Messages

For more tailored interactions, you can define system messages that provide context or instructions to the AI model. These messages are divided into two categories: main system messages and add-on messages.

- **Main System Messages**: Set general instructions or roles for the AI, like "You are a helpful assistant."
- **Add-On Messages**: Specify additional guidelines, such as "Use Markdown markup."

#### Managing System Messages

- Use `M-x vips-select-main-system-message` and `M-x vips-select-addon-message` to choose main and add-on messages respectively.
- The selected messages are combined automatically and sent with your text to the AI.
- Clear selected messages with `M-x vips-clear-selected-message` if you wish to reset the AI's context.
- View the current combined message with `M-x vips-display-selected-messages`.

### Using OpenAI's Text-to-Speech (TTS)

Interact with OpenAI's text-to-speech model, enabling the output to be played within Emacs (if your version supports it) or through your operating system's default player.

#### TTS Keybindings

- `C-c .`: Play the selected text in your operating system's default player.

### Translating Text with DeepL

Translate text directly within Emacs using DeepL's powerful translation API.

- `C-c SPC`: Select output language, translate the selected text using DeepL, and append the translation.

### Local Language Model Support with LM Studio

`vips.el` also includes support for interacting with local language models (LLMs) using LM Studio. This feature allows users to leverage the power of LLMs hosted on their own infrastructure, providing greater control over data privacy and response times.

In fact, ny API that is compatible with OpenAI's GPT API can also be used effortlessly with vips.el. To do this, simply treat your chosen API as an LM Studio model and configure the `vips.el` settings accordingly using the provided LM Studio configuration instructions below.

#### Setting Up LM Studio

The local language model support in `vips.el` is designed to work with [LM Studio](https://lmstudio.ai). Follow the installation and setup guidelines provided by [LM Studio](https://lmstudio.ai) to set it up on your computer or server. Once you have successfully installed and run the service, ensure that it is accessible through the default URL `http://localhost:1234/v1/chat/completions`. If you wish to use a different address for your LM Studio instance, make sure to configure `vips.el` accordingly.

#### Configuring `vips.el` for Local LLMs

To configure `vips.el` to use your local LLM through LM Studio, you need to set the `vips-lmstudio-api-url` variable to the URL of your running LM Studio instance. By default, this is set to `http://localhost:1234/v1/chat/completions`. If your LM Studio instance is running on a different URL, update this variable accordingly:

```emacs-lisp
(setq vips-lmstudio-api-url "http://your-lm-studio-address:port/v1/chat/completions")
```

#### Using Local LLMs

Once LM Studio is running and `vips.el` is configured, you can interact with your local LLM using the following functions:

- `vips-chat-region-lmstudio-local-model`: Send selected text to your local LLM and insert the response at the end of the region.
- `mark-and-run-vips-chat-region-lmstudio-local-model-to-current`: Mark text from the start of the buffer to the current position and send it to your local LLM.
- `mark-and-run-vips-chat-region-lmstudio-local-model`: Send the entire buffer content to your local LLM.

These functions are integrated into `vips-mode` and can be invoked through keybindings or via `M-x`.

#### Keybindings for Local LLMs

The following keybindings are available when `vips-mode` is active, allowing for quick interaction with your local LLM:

- `C-c l`: Send selected text to the local LLM.
- `C-c C-l C-c`: Mark text from the start of the buffer to the current position and send it to the local LLM.
- `C-c C-l C-v`: Send the entire buffer content to the local LLM.

### Unbound Interactive Functions

Additional functions are available for invocation through `M-x` or by binding them to your own custom keybindings. Below is a list of interactive functions not included in the default keybindings:

#### Voice Selection
- `vips-select-voice`: Prompt the user to select a voice for text-to-speech synthesis from a predefined list of voices. The selected voice will be used for generating audio from text.

#### Language Selection for Translation
- `vips-select-language`: Allow the user to select a target language for translation via DeepL's API. The chosen language will be used for translating text from the buffer.

#### System Message Management
- `vips-clear-main-system-message`: Clear the currently selected main system message that is sent to OpenAI's API along with the user's prompt.
- `vips-clear-addon-message`: Clear the currently selected add-on message that can be appended to the main system message for additional context or instructions.
- `vips-clear-both-messages`: Clear both the selected main system message and the add-on message.
- `vips-clear-selected-message`: Prompt the user to select which message to clear: the main system message, the add-on message, or both.

#### System Message Selection
- `vips-select-main-system-message`: Prompt the user to select a main system message from a predefined list. This message provides context or instructions for OpenAI's API.
- `vips-select-addon-message`: Prompt the user to select an add-on message from a predefined list that can be combined with the main system message.

#### System Message Combination
- `vips-combine-messages`: Combine the selected main system message with the selected add-on message and store it in a global variable for use in API requests.

#### Speech Synthesis
- `vips-create-and-play-speech`: Generate speech from the selected text region and play it using Emacs' built-in audio playback capabilities.
- `vips-create-and-save-speech`: Generate speech from the selected text region and save it as an MP3 file to a specified location.

#### Speech Synthesis with OS Integration
- `vips-create-and-play-speech-with-os`: Generate speech from the selected text region, save it to a temporary file, and play it using the operating system's default media player.

These functions enhance your interaction with AI services by providing additional customization and control over the behavior of `vips.el`. To use these functions, you can either invoke them through `M-x` followed by the function name or bind them to custom keybindings in your Emacs configuration for quicker access.

### Configuration

#### API Keys

To use this tool, you must have valid API keys for GPT and/or DeepL.

Set your API keys in your Emacs configuration:

```emacs-lisp
(setq openai-api-key "your-openai-api-key")
(setq deepl-api-key "your-deepl-api-key")
```

Replace <your-openai-key-here> and <your-deepl-key-here> with your actual API keys.

#### Languages

The `vips-languages` variable contains the list of language codes available for the translation function, initially set to include Danish ("DA") and English ("EN"). To modify this list, you can add a `setq` form to your Emacs configuration file (.emacs or init.el), like so:

```emacs-lisp
(setq vips-languages '("EN" "DE" "FR" "ES")) ; Add desired language codes
```

#### System messages

Define system messages in your `config.el`:

```emacs-lisp
;; Set main system message
(setq vips-selected-main-system-message "You are a helpful assistant.")

;; Set add-on message
(setq vips-selected-addon-message "Use Org Mode markup.")

;; Combine messages
(vips-combine-messages)

;; Update messages in all buffers
(vips-update-system-message-in-all-buffers)
```

## Alternatives

`vips.el` is not the only Emacs client for ChatGPT. There are also: [GPTel](https://github.com/karthink/gptel), [chatgpt-shell](https://github.com/xenodium/chatgpt-shell), [org-ai](https://github.com/rksm/org-ai), [chatgpt-arcana](https://github.com/CarlQLange/chatgpt-arcana.el), [leafy-mode](https://github.com/MichaelBurge/leafy-mode), and [chat.el](https://github.com/iwahbe/chat.el).

## License

This program is distributed as free software. You can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. For more details, please refer to the [LICENSE](LICENSE) file.

## Acknowledgements

This project was inspired by `aide.el` by Junji Zhi. The POST-related code was adapted and extended from the original code in `aide.el`. I thank Junji Zhi for their contribution to the open source community. `aide.el` is licensed under the GPL license.
