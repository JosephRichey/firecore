Shiny.addCustomMessageHandler('firecore-read-auth-cookie', function(message) {
  var prefix = encodeURIComponent(message.cookie_name) + '=';
  var entry = document.cookie.split('; ').find(function(cookie) { return cookie.indexOf(prefix) === 0; });
  Shiny.setInputValue(message.input_id, entry ? decodeURIComponent(entry.slice(prefix.length)) : '', {priority: 'event'});
});
Shiny.addCustomMessageHandler('firecore-write-auth-cookie', function(message) {
  var secure = window.location.protocol === 'https:' ? '; Secure' : '';
  document.cookie = encodeURIComponent(message.cookie_name) + '=' + encodeURIComponent(message.value) + '; Max-Age=' + message.max_age + '; Path=/; SameSite=Lax' + secure;
});
