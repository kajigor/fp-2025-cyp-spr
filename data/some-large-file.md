# Header 1: Main Title

This is a paragraph introducing the document. It contains some **bold text** and some *italic text*. We can also have
***bold and italic*** text. Let's also include an inline `code` snippet.

## Header 2: Lists and Links

Here's an example of an unordered (bullet) list:

* Item A
    * Sub-item A1
        * Sub-sub-item A1a
        * Sub-sub-item A1b (with a [link to Google](https://www.google.com))
    * Sub-item A2: This sub-item has a `code span` and **bolded text**.
* Item B
* Item C: This item contains an image: ![Placeholder Image](https://placehold.co/100x50/000000/FFFFFF?text=Image)

And here's an ordered (numbered) list:

1. First item
    1. Nested first item
        1. Deeply nested item 1.1.1
    2. Nested second item with *emphasis*.
2. Second item
    * Can mix with unordered lists
    * Another item
        1. And back to ordered
3. Third item: [Another link, this time to OpenAI](https://www.openai.com)

---

## Header 2: Emphasis, Strong, and Other Inline Formatting

Let's explore more formatting.

This sentence has **strong emphasis (bold)**.
This sentence has *regular emphasis (italic)*.
This sentence has ***strong and regular emphasis (bold and italic)***.
We can also use underscores for _italic_ and __bold__. Or even combine them like __*this*__ or _**this**_.

Strikethrough is done with two tildes: ~~this text is strikethrough~~.

Inline code can be `const example = "hello";` or `<html>`.

## Header 2: Code Blocks

## Header 2: Horizontal Rules

Horizontal rules are created by three or more hyphens, asterisks, or underscores.

---

They can be used to separate sections of content.

## Header 2: Nested Structures and Complex Combinations

Let's combine various elements.

1. **First ordered item with bold text.**
    * Unordered sub-item with a link: [Markdown Guide](https://www.markdownguide.org)
    * Another unordered sub-item with `inline code`.
2. Second ordered item.

### And the code block

```python
# A python code block inside a list item
class MyClass:
  def __init__(self, name):
      self.name = name
   
  def greet(self):
      print(f"Hello, {self.name}")

obj = MyClass("Parser")
obj.greet()
```
