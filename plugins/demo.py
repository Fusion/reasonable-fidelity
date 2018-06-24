# A few silly examples


## First, this is where we declare which capabilities this plugin can handle

def get_capabilities():
  return [
    "should_ignore_response",
    "should_ignore_attribute",
    "bogus_capability"
  ]


## A capability: ignoring a url's response if it happens to be google(!)

def should_ignore_response(url, mime_type):
  if url == "https://google.com":
    return True
  return False


## A capability: ignoring an attribute if it is named 'example'

def should_ignore_attribute(attribute_name):
  if attribute_name == "example":
    return True
  return False
