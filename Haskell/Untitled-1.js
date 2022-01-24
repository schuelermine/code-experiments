Synthesizing 10/10 solutions

=======

//
// var checkbox = document.createElement('checkbox');
// checkbox.innerHTML = 'Checkbox';
// checkbox.addEventListener('change', function(e) {
//   console.log('Checkbox changed!');
// });
// document.body.appendChild(checkbox);
//
//
// There are two ways to use this.
//
// 1. If you want to create a custom element that will be used in your page,
//    you can use the `customElement` method.
//
// 2. If you want to create a custom element that will be used in another
//    page, you can use the `customElement` method and register it with
//    `document.registerElement` instead of `document.createElement`.
//
//    This is useful if you want to avoid having to register the element
//    with `document.registerElement` and instead register it with
//    `document.registerElement` in the page where it is used.
//
//    This is useful if you want to avoid having to register the element
//    with `document.registerElement` and instead register it with
//    `document.registerElement` in the page where it is used.
//
//    This is useful if you want to avoid having to register the element
//    with `document.registerElement` and instead register it with
//    `document.registerElement` in the page where it is used.
//
//    This is useful if you want to avoid having to register the element
//    with `document.registerElement` and instead register it with
//    `document.registerElement` in the page where it is used.
//
//    This is useful if you want to avoid having to register the element
//    with `document.registerElement` and instead register it with
//    `document.registerElement` in the page where it is used.
//
//    This is useful if you want to avoid having to register the element
//    with `document.registerElement` and instead register it with
//    `document.registerElement` in the page where it is used.
//
//    This is useful if you want to avoid having to register the element
//    with `document.register

=======


import {register} from 'webcomponents/webcomponents-lite.js';

class Checkbox extends HTMLElement {
  constructor() {
    super();
    this.value = false;
  }

  connectedCallback() {
    this.addEventListener('change', () => {
      this.value = !this.value;
    });
  }

  get checked() {
    return this.value;
  }

  set checked(value) {
    this.value = value;
  }
}

// Register the element
register('my-checkbox', Checkbox);

// Use the element
<my-checkbox checked></my-checkbox>
```

## Web Component polyfills

Web Components are a new standard which is not yet supported by all browsers. To provide a fallback for unsupported browsers, we use polyfills.

The polyfills provided by webcomponentsjs are available as npm packages.

### Polyfill for Custom Elements v1

```sh
npm install --save

=======

//
// The html file:
// <checkbox id="my-checkbox" checked></checkbox>
//
// The javascript:
//
//  var myCheckbox = document.getElementById('my-checkbox');
//  myCheckbox.addEventListener('change', function(e) {
//    console.log(e.target.checked);
//  });
//
// The result:
//
//  true
//
// The html file:
// <checkbox id="my-checkbox" checked></checkbox>
//
// The javascript:
//
//  var myCheckbox = document.getElementById('my-checkbox');
//  myCheckbox.addEventListener('change', function(e) {
//    console.log(e.target.checked);
//  });
//
// The result:
//
//  false
//
// The html file:
// <checkbox id="my-checkbox"></checkbox>
//
// The javascript:
//
//  var myCheckbox = document.getElementById('my-checkbox');
//  myCheckbox.addEventListener('change', function(e) {
//    console.log(e.target.checked);
//  });
//
// The result:
//
//  false
//
// The html file:
// <checkbox id="my-checkbox" checked></checkbox>
//
// The javascript:
//
//  var myCheckbox = document.getElementById('my-checkbox');
//  myCheckbox.addEventListener('change', function(e) {
//    console.log(e.target.checked);
//  });
//
// The result:
//
//  true
//
// The html file:
// <checkbox id="my-checkbox"></checkbox>
//
// The javascript:
//
//  var myCheckbox = document.getElementById('my-checkbox');
//  myCheckbox.addEventListener('change', function(e) {
//    console.log(e.target.checked);
//  });
//
// The result:
//
//  false

=======

//
// <checkbox-input id="c" name="c" value="v" />
//
// Usage:
// <checkbox-input id="c" name="c" value="v" checked="true" />
//
// Attributes:
// id - The id of the input element.
// name - The name of the input element, used for form submission.
// value - The value of the input element.
// checked - A boolean value that determines if the input element is checked.
//
// Events:
// onchange - When the input element changes its value.
//
// Dependencies:
// - checkbox-input.js
// - checkbox-input.css
//
// Example:
// <script src="../dist/checkbox-input.js"></script>
// <script>
//   var checkboxInput = new CheckboxInput('c', 'c', 'v', true);
// </script>

(function(w) {
  'use strict';

  var CheckboxInput = function(id, name, value, checked) {
    this.id = id;
    this.name = name;
    this.value = value;
    this.checked = checked;
    this.element = null;
    this.onchange = null;
  };

  CheckboxInput.prototype = {
    constructor: CheckboxInput,

    create: function() {
      this.element = w.document.createElement('input');
      this.element.type = 'checkbox';
      this.element.id = this.id;
      this.element.name = this.name;
      this.element.value = this.value;
      this.element.checked = this.checked;
      this.element.addEventListener('change', function() {
        if (this.onchange) {
          this.onchange();
        }
      }.bind(this));
    },

    setValue: function(v) {
      this.value = v;
      this.element.value = v;
    },

    getValue: function() {
      return this.element.value;
    },

    setChecked: function(

=======

//
// The element is called "something-checkbox".
//
// The element has two custom attributes:
//   checked: boolean
//   disabled: boolean
//
// The element has one custom property:
//   checked: boolean
//
// The element has one event:
//   change: function(event) {}
//
// The element has one method:
//   checked: function() {}
(function(scope) {
  'use strict';

  // Register a webcomponents custom element for a checkbox.
  function SomethingCheckbox(document) {
    this.document_ = document;
    this.handleClick_ = this.handleClick_.bind(this);
  }
  window.customElements.define('something-checkbox', SomethingCheckbox);

  // Set the default values.
  SomethingCheckbox.prototype.detachedCallback = function() {
    this.checked = false;
    this.disabled = false;
  };

  // Handle the click event.
  SomethingCheckbox.prototype.handleClick_ = function(event) {
    this.checked = !this.checked;
    this.dispatchEvent(new CustomEvent('change', {
      bubbles: true,
      cancelable: true,
      detail: {
        checked: this.checked
      }
    }));
  };

  // Set the checked property.
  SomethingCheckbox.prototype.get checked() {
    return this.hasAttribute('checked');
  };
  SomethingCheckbox.prototype.set checked(value) {
    if (value) {
      this.setAttribute('checked', '');
    } else {
      this.removeAttribute('checked');
    }
  };

  // Set the disabled property.
  SomethingCheckbox.prototype.get disabled() {
    return this.hasAttribute('disabled');
  };
  SomethingCheckbox.prototype.set disabled(value) {
    if (value) {
      this.setAttribute('disabled', '');
    } else {
      this.removeAttribute('disabled');
    }
  };

  // Set the checked property.
  SomethingCheckbox.prototype.get checked() {
    return this.

=======

//
// <checkbox-name></checkbox-name>
//
// Usage:
// <checkbox-name checked="{{true|false}}">
//   <label>
//     {{'Checkbox Label'|trans}}
//   </label>
// </checkbox-name>
//
// <checkbox-name></checkbox-name>

(function(scope) {
  'use strict';

  function CheckboxName(el, binding, vnode) {
    var checked = binding.value;
    var name = el.getAttribute('name');
    var onChange = function(e) {
      checked = e.target.checked;
      binding.set(checked);
    };
    el.addEventListener('change', onChange);
    vnode.data.onunbind = function() {
      el.removeEventListener('change', onChange);
    };
    scope.dom.setAttribute(el, 'name', name);
  }

  scope.bind(CheckboxName);
})(webComponents);

=======

//
// This will be used in the html file like this:
//
// <my-checkbox></my-checkbox>
//
// The element will be defined in the file something.js
//
// The element will be registered in the file something.js
//
// The element will be available in the file something.html
// after the Polymer library has been loaded.
Polymer({
  is: 'my-checkbox',
  properties: {
    label: {
      type: String,
      value: '',
      notify: true
    }
  },
  listeners: {
    'change': 'onChange'
  },
  onChange: function(event) {
    console.log(event.target.checked);
  }
});

=======

//
// This is a simple example of how to use webcomponents.js to register a custom
// element. It is not meant to be a tutorial on webcomponents.
//
// The element is called "checkbox".
//
// Usage:
// <checkbox value="{{value}}" on-change="onChange(value)"></checkbox>
//
// The on-change attribute is called when the checkbox changes.
// The value attribute is the value of the checkbox.

(function() {
    'use strict';

    // The name of the custom element.
    var name = 'checkbox';

    // The checkbox element definition.
    var definition = {
        // The name of the custom element.
        name: name,

        // The properties of the custom element.
        properties: {
            value: {
                type: Boolean,
                value: false,
                reflectToAttribute: true
            }
        },

        // The methods of the custom element.
        methods: {
            onChange: function(value) {
                this.value = value;
            }
        },

        // The attached callback is called when the element is attached to the
        // DOM.
        attached: function() {
            // Set the initial value of the checkbox.
            this.value = this.hasAttribute('value');
        }
    };

    // Register the custom element.
    window.customElements.define(name, definition);
})();

=======

//
// This is a very simple example of a custom element.
//
// The element is called 'checkbox'.
//
// The element is a simple html checkbox.
//
// The element is created by the 'checkbox-element' function.
//
// The element is used by the 'checkbox-demo' example.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox-element' function.
//
// The element is registered in the 'html' section.
//
// The element is created by the 'checkbox

=======

//
// The element is called "something" and it has an attribute "my-attribute".
// The element has a property "my-property".
//
// The element is registered in a custom element registry.

Polymer({
  is: 'something',
  properties: {
    myProperty: {
      type: String,
      value: 'Hello world'
    }
  },
  behaviors: [
    Polymer.NeonAnimatableBehavior
  ],
  listeners: {
    'neon-animation-finish': 'onNeonAnimationFinish'
  },
  onNeonAnimationFinish: function(event) {
    console.log(event.detail.name);
  }
});
