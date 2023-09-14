# vips.el

`vips.el` is a simple but powerful Emacs interface for OpenAI's GPT API and DeepL's translation API. This tool allows you to leverage the capabilities of these APIs directly within your Emacs environment.

## Key Features

- Work directly with OpenAI's GPT-4 and GPT-3.5-turbo models in Emacs.
- Customize parameters for OpenAI's API, including max tokens, temperature, top-p, frequency penalty, and presence penalty to fine-tune the output.
- Translate text seamlessly using the DeepL API.
- Choose your target language for translation.

## Installation Steps

1. Download `vips.el` to your local system.
2. Add the directory containing `vips.el` to your `load-path` using `(add-to-list 'load-path "/path/to/your/directory")`.
3. Load the package with `(require 'vips)`.

## How to Use

Activate `vips-mode` in your current buffer by executing `M-x vips-mode`.

Once `vips-mode` is enabled, you can use the following shortcuts:

- `C-c <left>`: Send selected region or paragraph to OpenAI's GPT-4 model and append the result at the end of the region.
- `C-c <right>`: Send selected region or paragraph to OpenAI's GPT-3.5-turbo model and append the result at the end of the region.
- `C-c <down>`: Send selected region or paragraph to a user-selected OpenAI model and append the result at the end of the region.
- `C-c C-a C-v`: Send entire buffer to OpenAI's GPT-4 model and append the result at the end of the buffer.
- `C-c C-a C-c`: Send region defined from the top of the buffer to the current point to OpenAI's GPT-4 model and append the result at the end of the region.
- `C-c SPC`: Select output language and translate selected region or paragraph using DeepL API and append the result at the end of the region.

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
