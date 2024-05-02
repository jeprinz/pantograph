export const fromInputEventToTargetValue = (event) => () => event.target.value

export const get_url_search_param = (name) => () => {
	const urlParams = new URLSearchParams(window.location.search);
	return urlParams.get(name) ?? "";
}

export const encode_uri_string = (str) => encodeURIComponent(str)

export const navigator_clipboard_text_ = async () => navigator.clipboard.readText()