!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function f(t){return r(2,t,function(r){return function(n){return t(r,n)}})}function t(u){return r(3,u,function(t){return function(r){return function(n){return u(t,r,n)}}})}function u(e){return r(4,e,function(u){return function(t){return function(r){return function(n){return e(u,t,r,n)}}}})}function e(i){return r(5,i,function(e){return function(u){return function(t){return function(r){return function(n){return i(e,u,t,r,n)}}}}})}function i(o){return r(6,o,function(i){return function(e){return function(u){return function(t){return function(r){return function(n){return o(i,e,u,t,r,n)}}}}}})}function o(f){return r(7,f,function(o){return function(i){return function(e){return function(u){return function(t){return function(r){return function(n){return f(o,i,e,u,t,r,n)}}}}}}})}function a(a){return r(8,a,function(f){return function(o){return function(i){return function(e){return function(u){return function(t){return function(r){return function(n){return a(f,o,i,e,u,t,r,n)}}}}}}}})}function c(c){return r(9,c,function(a){return function(f){return function(o){return function(i){return function(e){return function(u){return function(t){return function(r){return function(n){return c(a,f,o,i,e,u,t,r,n)}}}}}}}}})}function b(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function s(n,r,t,u){return 3===n.a?n.f(r,t,u):n(r)(t)(u)}function l(n,r,t,u,e){return 4===n.a?n.f(r,t,u,e):n(r)(t)(u)(e)}function v(n,r,t,u,e,i){return 5===n.a?n.f(r,t,u,e,i):n(r)(t)(u)(e)(i)}function h(n,r,t,u,e,i,o){return 6===n.a?n.f(r,t,u,e,i,o):n(r)(t)(u)(e)(i)(o)}function d(n,r){for(var t,u=[],e=$(n,r,0,u);e&&(t=u.pop());e=$(t.a,t.b,0,u));return e}function $(n,r,t,u){if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&O(5),!1;if(100<t)return u.push({a:n,b:r}),!0;for(var e in n.$<0&&(n=pr(n),r=pr(r)),n)if(!$(n[e],r[e],t+1,u))return!1;return!0}f(d),f(function(n,r){return!d(n,r)});function g(n,r,t){if("object"!=typeof n)return n===r?0:n<r?-1:1;if(void 0===n.$)return(t=g(n.a,r.a))||(t=g(n.b,r.b))?t:g(n.c,r.c);for(;n.b&&r.b&&!(t=g(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}f(function(n,r){return g(n,r)<0}),f(function(n,r){return g(n,r)<1}),f(function(n,r){return 0<g(n,r)}),f(function(n,r){return 0<=g(n,r)}),f(function(n,r){r=g(n,r);return r<0?dr:r?hr:lr});var p=0;function m(n,r){var t,u={};for(t in n)u[t]=n[t];for(t in r)u[t]=r[t];return u}f(y);function y(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t={$:1,a:n.a,b:r};n=n.b;for(var u=t;n.b;n=n.b)u=u.b={$:1,a:n.a,b:r};return t}var A={$:0};function j(n,r){return{$:1,a:n,b:r}}var w=f(j);function k(n){for(var r=A,t=n.length;t--;)r={$:1,a:n[t],b:r};return r}function N(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}var x=t(function(n,r,t){for(var u=[];r.b&&t.b;r=r.b,t=t.b)u.push(b(n,r.a,t.a));return k(u)});u(function(n,r,t,u){for(var e=[];r.b&&t.b&&u.b;r=r.b,t=t.b,u=u.b)e.push(s(n,r.a,t.a,u.a));return k(e)}),e(function(n,r,t,u,e){for(var i=[];r.b&&t.b&&u.b&&e.b;r=r.b,t=t.b,u=u.b,e=e.b)i.push(l(n,r.a,t.a,u.a,e.a));return k(i)}),i(function(n,r,t,u,e,i){for(var o=[];r.b&&t.b&&u.b&&e.b&&i.b;r=r.b,t=t.b,u=u.b,e=e.b,i=i.b)o.push(v(n,r.a,t.a,u.a,e.a,i.a));return k(o)}),f(function(t,n){return k(N(n).sort(function(n,r){return g(t(n),t(r))}))}),f(function(t,n){return k(N(n).sort(function(n,r){r=b(t,n,r);return r===lr?0:r===dr?-1:1}))});var C=t(function(n,r,t){for(var u=Array(n),e=0;e<n;e++)u[e]=t(r+e);return u}),_=f(function(n,r){for(var t=Array(n),u=0;u<n&&r.b;u++)t[u]=r.a,r=r.b;return t.length=u,{a:t,b:r}}),E=(f(function(n,r){return r[n]}),t(function(n,r,t){for(var u=t.length,e=Array(u),i=0;i<u;i++)e[i]=t[i];return e[n]=r,e}),f(function(n,r){for(var t=r.length,u=Array(t+1),e=0;e<t;e++)u[e]=r[e];return u[t]=n,u}),t(function(n,r,t){for(var u=t.length,e=0;e<u;e++)r=b(n,t[e],r);return r}),t(function(n,r,t){for(var u=t.length-1;0<=u;u--)r=b(n,t[u],r);return r}));f(function(n,r){for(var t=r.length,u=Array(t),e=0;e<t;e++)u[e]=n(r[e]);return u}),t(function(n,r,t){for(var u=t.length,e=Array(u),i=0;i<u;i++)e[i]=b(n,r+i,t[i]);return e}),t(function(n,r,t){return t.slice(n,r)}),t(function(n,r,t){for(var u=r.length,e=n-u,i=Array(u+(e=t.length<e?t.length:e)),o=0;o<u;o++)i[o]=r[o];for(o=0;o<e;o++)i[o+u]=t[o];return i}),f(function(n,r){return r}),f(function(n,r){return console.log(n+": <internals>"),r});function O(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}f(function(n,r){return n+r}),f(function(n,r){return n-r}),f(function(n,r){return n*r}),f(function(n,r){return n/r}),f(function(n,r){return n/r|0});var L=f(Math.pow),M=(f(function(n,r){return r%n}),f(function(n,r){r%=n;return 0===n?O(11):0<r&&n<0||r<0&&0<n?r+n:r}),Math.cos),S=Math.sin,T=Math.tan;f(Math.atan2);var z=Math.ceil,q=Math.floor,F=Math.log,J=isNaN;f(function(n,r){return n&&r}),f(function(n,r){return n||r}),f(function(n,r){return n!==r});var B=f(function(n,r){return n+r});f(function(n,r){return n+r});f(function(n,r){for(var t=r.length,u=Array(t),e=0;e<t;){var i=r.charCodeAt(e);i<55296||56319<i?(u[e]=n(r[e]),e++):(u[e]=n(r[e]+r[e+1]),e+=2)}return u.join("")});var D=f(function(n,r){for(var t=[],u=r.length,e=0;e<u;){var i=r[e],o=r.charCodeAt(e);e++,o<55296||56319<o||(i+=r[e],e++),n(i)&&t.push(i)}return t.join("")});t(function(n,r,t){for(var u=t.length,e=0;e<u;){var i=t[e],o=t.charCodeAt(e);e++,o<55296||56319<o||(i+=t[e],e++),r=b(n,i,r)}return r});var P=t(function(n,r,t){for(var u=t.length;u--;){var e=t[u],i=t.charCodeAt(u);r=b(n,e=i>=56320&&57343>=i?t[--u]+e:e,r)}return r}),R=f(function(n,r){return r.split(n)}),I=f(function(n,r){return r.join(n)}),W=t(function(n,r,t){return t.slice(n,r)});f(function(n,r){for(var t=r.length;t--;){var u=r[t],e=r.charCodeAt(t);if(n(u=e>=56320&&57343>=e?r[--t]+u:u))return!0}return!1});var X=f(function(n,r){for(var t=r.length;t--;){var u=r[t],e=r.charCodeAt(t);if(!n(u=e>=56320&&57343>=e?r[--t]+u:u))return!1}return!0}),Y=f(function(n,r){return!!~r.indexOf(n)}),Z=f(function(n,r){return 0==r.indexOf(n)}),G=(f(function(n,r){return n.length<=r.length&&r.lastIndexOf(n)==r.length-n.length}),f(function(n,r){var t=n.length;if(t<1)return A;for(var u=0,e=[];-1<(u=r.indexOf(n,u));)e.push(u),u+=t;return k(e)}));function H(n){return n+""}f(function(n,r){return{$:6,d:n,b:r}}),f(function(n,r){return{$:7,e:n,b:r}});f(function(n,r){return{$:10,b:r,h:n}});var K=f(function(n,r){return{$:9,f:n,g:[r]}}),Q=t(function(n,r,t){return{$:9,f:n,g:[r,t]}}),U=(u(function(n,r,t,u){return{$:9,f:n,g:[r,t,u]}}),e(function(n,r,t,u,e){return{$:9,f:n,g:[r,t,u,e]}}),i(function(n,r,t,u,e,i){return{$:9,f:n,g:[r,t,u,e,i]}}),o(function(n,r,t,u,e,i,o){return{$:9,f:n,g:[r,t,u,e,i,o]}}),a(function(n,r,t,u,e,i,o,f){return{$:9,f:n,g:[r,t,u,e,i,o,f]}}),c(function(n,r,t,u,e,i,o,f,a){return{$:9,f:n,g:[r,t,u,e,i,o,f,a]}}),f(function(n,r){try{return V(n,JSON.parse(r))}catch(n){return yr(b(Ar,"This is not valid JSON! "+n.message,r))}}),f(V));function V(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?kr(n.c):un("null",r);case 3:return rn(r)?nn(n.b,r,k):un("a LIST",r);case 4:return rn(r)?nn(n.b,r,tn):un("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return un("an OBJECT with a field named `"+t+"`",r);var u=V(n.b,r[t]);return ot(u)?u:yr(b(jr,t,u.a));case 7:t=n.e;if(!rn(r))return un("an ARRAY",r);if(r.length<=t)return un("a LONGER array. Need index "+t+" but only see "+r.length+" entries",r);u=V(n.b,r[t]);return ot(u)?u:yr(b(wr,t,u.a));case 8:if("object"!=typeof r||null===r||rn(r))return un("an OBJECT",r);var e,i=A;for(e in r)if(r.hasOwnProperty(e)){u=V(n.b,r[e]);if(!ot(u))return yr(b(jr,e,u.a));i={$:1,a:{a:e,b:u.a},b:i}}return kr(Br(i));case 9:for(var o=n.f,f=n.g,a=0;a<f.length;a++){u=V(f[a],r);if(!ot(u))return u;o=o(u.a)}return kr(o);case 10:u=V(n.b,r);return ot(u)?V(n.h(u.a),r):u;case 11:for(var c=A,v=n.g;v.b;v=v.b){u=V(v.a,r);if(ot(u))return u;c={$:1,a:u.a,b:c}}return yr(Nr(Br(c)));case 1:return yr(b(Ar,n.a,r));case 0:return kr(n.a)}}function nn(n,r,t){for(var u=r.length,e=Array(u),i=0;i<u;i++){var o=V(n,r[i]);if(!ot(o))return yr(b(wr,i,o.a));e[i]=o.a}return kr(t(e))}function rn(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function tn(r){return b(it,r.length,function(n){return r[n]})}function un(n,r){return yr(b(Ar,"Expecting "+n,r))}function en(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return en(n.b,r.b);case 6:return n.d===r.d&&en(n.b,r.b);case 7:return n.e===r.e&&en(n.b,r.b);case 9:return n.f===r.f&&on(n.g,r.g);case 10:return n.h===r.h&&en(n.b,r.b);case 11:return on(n.g,r.g)}}function on(n,r){var t=n.length;if(t!==r.length)return!1;for(var u=0;u<t;u++)if(!en(n[u],r[u]))return!1;return!0}var fn=f(function(n,r){return JSON.stringify(r,null,n)+""});t(function(n,r,t){return t[n]=r,t});function an(n){return{$:0,a:n}}var cn=f(function(n,r){return{$:3,b:n,d:r}});f(function(n,r){return{$:4,b:n,d:r}});var vn=0;function bn(n){n={$:0,e:vn++,f:n,g:null,h:[]};return gn(n),n}function sn(r){return{$:2,b:function(n){n({$:0,a:bn(r)})},c:null}}function ln(n,r){n.h.push(r),gn(n)}var hn=f(function(r,t){return{$:2,b:function(n){ln(r,t),n({$:0,a:p})},c:null}});var dn=!1,$n=[];function gn(n){if($n.push(n),!dn){for(dn=!0;n=$n.shift();)!function(r){for(;r.f;){var n=r.f.$;if(0===n||1===n){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else{if(2===n)return r.f.c=r.f.b(function(n){r.f=n,gn(r)});if(5===n){if(0===r.h.length)return;r.f=r.f.b(r.h.shift())}else r.g={$:3===n?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}}(n);dn=!1}}u(function(n,r,t,u){return pn(r,u,n.a9,n.bl,n.bj,function(){return function(){}})});function pn(n,r,t,u,e,i){var o=b(U,n,r?r.flags:void 0);ot(o)||O(2);var f={},a=(o=t(o.a)).a,c=i(v,a),i=function(n,r){var t,u;for(u in mn){var e=mn[u];e.a&&((t=t||{})[u]=e.a(u,r)),n[u]=function(n,r){var u={g:r,h:void 0},e=n.c,i=n.d,o=n.e,f=n.f;return u.h=bn(b(cn,function n(t){return b(cn,n,{$:5,b:function(n){var r=n.a;return 0===n.$?s(i,u,r,t):o&&f?l(e,u,r.i,r.j,t):s(e,u,o?r.i:r.j,t)}})},n.b))}(e,r)}return t}(f,v);function v(n,r){o=b(u,n,a),c(a=o.a,r),Nn(f,o.b,e(a))}return Nn(f,o.b,e(a)),i?{ports:i}:{}}var mn={};var yn=f(function(r,t){return{$:2,b:function(n){r.g(t),n({$:0,a:p})},c:null}});f(function(n,r){return b(hn,n.h,{$:0,a:r})});function An(r){return function(n){return{$:1,k:r,l:n}}}function jn(n){return{$:2,m:n}}f(function(n,r){return{$:3,n:n,o:r}});var wn=[],kn=!1;function Nn(n,r,t){if(wn.push({p:n,q:r,r:t}),!kn){kn=!0;for(var u;u=wn.shift();)!function(n,r,t){var u,e={};for(u in xn(!0,r,e,null),xn(!1,t,e,null),n)ln(n[u],{$:"fx",a:e[u]||{i:A,j:A}})}(u.p,u.q,u.r);kn=!1}}function xn(n,r,t,u){switch(r.$){case 1:var e=r.k,i=function(n,r,t,u){return b(n?mn[r].e:mn[r].f,function(n){for(var r=t;r;r=r.t)n=r.s(n);return n},u)}(n,e,u,r.l);return void(t[e]=function(n,r,t){return t=t||{i:A,j:A},n?t.i={$:1,a:r,b:t.i}:t.j={$:1,a:r,b:t.j},t}(n,i,t[e]));case 2:for(var o=r.m;o.b;o=o.b)xn(n,o.a,t,u);return;case 3:return void xn(n,r.o,t,{s:r.n,t:u})}}f(function(n,r){return r});var Cn;f(function(r,t){return function(n){return r(t(n))}});var _n="undefined"!=typeof document?document:{};u(function(n,r,t,u){u=u.node;return u.parentNode.replaceChild(Bn(n,function(){}),u),{}});function En(n){return{$:0,a:n}}var On=f(function(i,o){return f(function(n,r){for(var t=[],u=0;r.b;r=r.b){var e=r.a;u+=e.b||0,t.push(e)}return u+=t.length,{$:1,c:o,d:Fn(n),e:t,f:i,b:u}})})(void 0);f(function(i,o){return f(function(n,r){for(var t=[],u=0;r.b;r=r.b){var e=r.a;u+=e.b.b||0,t.push(e)}return u+=t.length,{$:2,c:o,d:Fn(n),e:t,f:i,b:u}})})(void 0);f(function(n,r){return{$:4,j:n,k:r,b:1+(r.b||0)}});f(function(n,r){return{$:5,l:[n,r],m:function(){return n(r)},k:void 0}}),t(function(n,r,t){return{$:5,l:[n,r,t],m:function(){return b(n,r,t)},k:void 0}}),u(function(n,r,t,u){return{$:5,l:[n,r,t,u],m:function(){return s(n,r,t,u)},k:void 0}}),e(function(n,r,t,u,e){return{$:5,l:[n,r,t,u,e],m:function(){return l(n,r,t,u,e)},k:void 0}}),i(function(n,r,t,u,e,i){return{$:5,l:[n,r,t,u,e,i],m:function(){return v(n,r,t,u,e,i)},k:void 0}}),o(function(n,r,t,u,e,i,o){return{$:5,l:[n,r,t,u,e,i,o],m:function(){return h(n,r,t,u,e,i,o)},k:void 0}}),a(function(n,r,t,u,e,i,o,f){return{$:5,l:[n,r,t,u,e,i,o,f],m:function(){return function(n,r,t,u,e,i,o,f){return 7===n.a?n.f(r,t,u,e,i,o,f):n(r)(t)(u)(e)(i)(o)(f)}(n,r,t,u,e,i,o,f)},k:void 0}}),c(function(n,r,t,u,e,i,o,f,a){return{$:5,l:[n,r,t,u,e,i,o,f,a],m:function(){return function(n,r,t,u,e,i,o,f,a){return 8===n.a?n.f(r,t,u,e,i,o,f,a):n(r)(t)(u)(e)(i)(o)(f)(a)}(n,r,t,u,e,i,o,f,a)},k:void 0}});var Ln=f(function(n,r){return{$:"a0",n:n,o:r}}),Mn=f(function(n,r){return{$:"a1",n:n,o:r}}),Sn=(f(function(n,r){return{$:"a2",n:n,o:r}}),f(function(n,r){return{$:"a3",n:n,o:r}}));t(function(n,r,t){return{$:"a4",n:r,o:{f:n,o:t}}});f(function(n,r){return"a0"===r.$?b(Ln,r.n,function(n,r){var t=vt(r);return{$:r.$,a:t?s(at,t<3?zn:qn,ct(n),r.a):b(ft,n,r.a)}}(n,r.o)):r});var Tn,zn=f(function(n,r){return{a:n(r.a),b:r.b}}),qn=f(function(n,r){return{s:n(r.s),Z:r.Z,W:r.W}});function Fn(n){for(var r={};n.b;n=n.b){var t=n.a,u=t.$,e=t.n,i=t.o;"a2"!==u?(t=r[u]||(r[u]={}),"a3"===u&&"class"===e?Jn(t,e,i):t[e]=i):"className"===e?Jn(r,e,i):r[e]=i}return r}function Jn(n,r,t){var u=n[r];n[r]=u?u+" "+t:t}function Bn(n,r){var t=n.$;if(5===t)return Bn(n.k||(n.k=n.m()),r);if(0===t)return _n.createTextNode(n.a);if(4===t){for(var u=n.k,e=n.j;4===u.$;)"object"!=typeof e?e=[e,u.j]:e.push(u.j),u=u.k;var i={j:e,p:r};return(o=Bn(u,i)).elm_event_node_ref=i,o}if(3===t)return Dn(o=n.h(n.g),r,n.d),o;var o=n.f?_n.createElementNS(n.f,n.c):_n.createElement(n.c);Cn&&"a"==n.c&&o.addEventListener("click",Cn(o)),Dn(o,r,n.d);for(var f=n.e,a=0;a<f.length;a++)o.appendChild(Bn(1===t?f[a]:f[a].b,r));return o}function Dn(n,r,t){for(var u in t){var e=t[u];"a1"===u?function(n,r){var t,u=n.style;for(t in r)u[t]=r[t]}(n,e):"a0"===u?function(n,r,t){var u,e=n.elmFs||(n.elmFs={});for(u in t){var i=t[u],o=e[u];if(i){if(o){if(o.q.$===i.$){o.q=i;continue}n.removeEventListener(u,o)}o=function(a,n){function c(n){var r=c.q,t=V(r.a,n);if(ot(t)){for(var u,e=vt(r),r=t.a,i=e?e<3?r.a:r.s:r,t=1==e?r.b:3==e&&r.Z,o=(t&&n.stopPropagation(),(2==e?r.b:3==e&&r.W)&&n.preventDefault(),a);u=o.j;){if("function"==typeof u)i=u(i);else for(var f=u.length;f--;)i=u[f](i);o=o.p}o(i,t)}}return c.q=n,c}(r,i),n.addEventListener(u,o,Tn&&{passive:vt(i)<2}),e[u]=o}else n.removeEventListener(u,o),e[u]=void 0}}(n,r,e):"a3"===u?function(n,r){for(var t in r){var u=r[t];void 0!==u?n.setAttribute(t,u):n.removeAttribute(t)}}(n,e):"a4"===u?function(n,r){for(var t in r){var u=r[t],e=u.f,u=u.o;void 0!==u?n.setAttributeNS(e,t,u):n.removeAttributeNS(e,t)}}(n,e):("value"!==u&&"checked"!==u||n[u]!==e)&&(n[u]=e)}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Tn=!0}}))}catch(n){}function Pn(n,r){var t=[];return In(n,r,t,0),t}function Rn(n,r,t,u){u={$:r,r:t,s:u,t:void 0,u:void 0};return n.push(u),u}function In(n,r,t,u){if(n!==r){var e=n.$,i=r.$;if(e!==i){if(1!==e||2!==i)return void Rn(t,0,u,r);r=function(n){for(var r=n.e,t=r.length,u=Array(t),e=0;e<t;e++)u[e]=r[e].b;return{$:1,c:n.c,d:n.d,e:u,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var o=n.l,f=r.l,a=o.length,c=a===f.length;c&&a--;)c=o[a]===f[a];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return In(n.k,r.k,v,0),void(0<v.length&&Rn(t,1,u,v));case 4:for(var b=n.j,s=r.j,l=!1,h=n.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;for(var d=r.k;4===d.$;)l=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;return l&&b.length!==s.length?void Rn(t,0,u,r):((l?function(n,r){for(var t=0;t<n.length;t++)if(n[t]!==r[t])return!1;return!0}(b,s):b===s)||Rn(t,2,u,s),void In(h,d,t,u+1));case 0:return void(n.a!==r.a&&Rn(t,3,u,r.a));case 1:return void Wn(n,r,t,u,Yn);case 2:return void Wn(n,r,t,u,Zn);case 3:if(n.h!==r.h)return void Rn(t,0,u,r);v=Xn(n.d,r.d);v&&Rn(t,4,u,v);v=r.i(n.g,r.g);return void(v&&Rn(t,5,u,v))}}}function Wn(n,r,t,u,e){var i;n.c===r.c&&n.f===r.f?((i=Xn(n.d,r.d))&&Rn(t,4,u,i),e(n,r,t,u)):Rn(t,0,u,r)}function Xn(n,r,t){var u,e,i,o,f;for(e in n)"a1"!==e&&"a0"!==e&&"a3"!==e&&"a4"!==e?e in r?(i=n[e])===(o=r[e])&&"value"!==e&&"checked"!==e||"a0"===t&&function(n,r){return n.$==r.$&&en(n.a,r.a)}(i,o)||((u=u||{})[e]=o):(u=u||{})[e]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[e].f,o:void 0}:"string"==typeof n[e]?"":null:(o=Xn(n[e],r[e]||{},e))&&((u=u||{})[e]=o);for(f in r)f in n||((u=u||{})[f]=r[f]);return u}function Yn(n,r,t,u){var e=n.e,i=r.e,n=e.length,r=i.length;r<n?Rn(t,6,u,{v:r,i:n-r}):n<r&&Rn(t,7,u,{v:n,e:i});for(var o=n<r?n:r,f=0;f<o;f++){var a=e[f];In(a,i[f],t,++u),u+=a.b||0}}function Zn(n,r,t,u){for(var e=[],i={},o=[],f=n.e,a=r.e,c=f.length,v=a.length,b=0,s=0,l=u;b<c&&s<v;){var h=f[b],d=a[s],$=h.a,g=d.a,p=h.b,m=d.b,y=void 0,A=void 0;if($!==g){var j,w,k,N,x=f[b+1],C=a[s+1];if(x&&(w=x.b,A=g===(j=x.a)),C&&(N=C.b,y=$===(k=C.a)),y&&A)In(p,N,e,++l),Hn(i,e,$,m,s,o),l+=p.b||0,Kn(i,e,$,w,++l),l+=w.b||0,b+=2,s+=2;else if(y)l++,Hn(i,e,g,m,s,o),In(p,N,e,l),l+=p.b||0,b+=1,s+=2;else if(A)Kn(i,e,$,p,++l),l+=p.b||0,In(w,m,e,++l),l+=w.b||0,b+=2,s+=1;else{if(!x||j!==k)break;Kn(i,e,$,p,++l),Hn(i,e,g,m,s,o),l+=p.b||0,In(w,N,e,++l),l+=w.b||0,b+=2,s+=2}}else In(p,m,e,++l),l+=p.b||0,b++,s++}for(;b<c;){p=(h=f[b]).b;Kn(i,e,h.a,p,++l),l+=p.b||0,b++}for(;s<v;){var _=_||[];Hn(i,e,(d=a[s]).a,d.b,void 0,_),s++}(0<e.length||0<o.length||_)&&Rn(t,8,u,{w:e,x:o,y:_})}var Gn="_elmW6BL";function Hn(n,r,t,u,e,i){var o=n[t];if(!o)return i.push({r:e,A:o={c:0,z:u,r:e,s:void 0}}),void(n[t]=o);if(1===o.c){i.push({r:e,A:o}),o.c=2;var f=[];return In(o.z,u,f,o.r),o.r=e,void(o.s.s={w:f,A:o})}Hn(n,r,t+Gn,u,e,i)}function Kn(n,r,t,u,e){var i=n[t];if(i){if(0===i.c){i.c=2;var o=[];return In(u,i.z,o,e),void Rn(r,9,e,{w:o,A:i})}Kn(n,r,t+Gn,u,e)}else{r=Rn(r,9,e,void 0);n[t]={c:1,z:u,r:e,s:r}}}function Qn(n,r,t,u){!function n(r,t,u,e,i,o,f){var a=u[e];var c=a.r;for(;c===i;){var v,b=a.$;if(1===b?Qn(r,t.k,a.s,f):8===b?(a.t=r,a.u=f,0<(v=a.s.w).length&&n(r,t,v,0,i,o,f)):9===b?(a.t=r,a.u=f,(b=a.s)&&(b.A.s=r,0<(v=b.w).length&&n(r,t,v,0,i,o,f))):(a.t=r,a.u=f),!(a=u[++e])||(c=a.r)>o)return e}var s=t.$;if(4===s){for(var l=t.k;4===l.$;)l=l.k;return n(r,l,u,e,i+1,o,r.elm_event_node_ref)}var h=t.e;var d=r.childNodes;for(var $=0;$<h.length;$++){var g=1===s?h[$]:h[$].b,p=++i+(g.b||0);if(i<=c&&c<=p&&(e=n(d[$],g,u,e,i,p,f),!(a=u[e])||(c=a.r)>o))return e;i=p}return e}(n,r,t,0,0,r.b,u)}function Un(n,r,t,u){return 0===t.length?n:(Qn(n,r,t,u),Vn(n,t))}function Vn(n,r){for(var t=0;t<r.length;t++){var u=r[t],e=u.t,u=function(n,r){switch(r.$){case 0:return function(n,r,t){var u=n.parentNode,t=Bn(r,t);t.elm_event_node_ref||(t.elm_event_node_ref=n.elm_event_node_ref);u&&t!==n&&u.replaceChild(t,n);return t}(n,r.s,r.u);case 4:return Dn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Vn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,u=0;u<t.i;u++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var e=(t=r.s).e,u=t.v,i=n.childNodes[u];u<e.length;u++)n.insertBefore(Bn(e[u],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var o=t.A;return void 0!==o.r&&n.parentNode.removeChild(n),o.s=Vn(n,t.w),n;case 8:return function(n,r){var t=r.s,u=function(n,r){if(n){for(var t=_n.createDocumentFragment(),u=0;u<n.length;u++){var e=n[u].A;t.appendChild(2===e.c?e.s:Bn(e.z,r.u))}return t}}(t.y,r);n=Vn(n,t.w);for(var e=t.x,i=0;i<e.length;i++){var o=e[i],f=o.A,f=2===f.c?f.s:Bn(f.z,r.u);n.insertBefore(f,n.childNodes[o.r])}u&&n.appendChild(u);return n}(n,r);case 5:return r.s(n);default:O(10)}}(e,u);e===n&&(n=u)}return n}function nr(n){if(3===n.nodeType)return{$:0,a:n.textContent};if(1!==n.nodeType)return{$:0,a:""};for(var r=A,t=n.attributes,u=t.length;u--;)var e=t[u],r={$:1,a:b(Sn,e.name,e.value),b:r};for(var i=n.tagName.toLowerCase(),o=A,f=n.childNodes,u=f.length;u--;)o={$:1,a:nr(f[u]),b:o};return s(On,i,r,o)}var rr=u(function(r,n,t,o){return pn(n,o,r.a9,r.bl,r.bj,function(t,n){var u=r.bm,e=o.node,i=nr(e);return ur(n,function(n){var r=u(n),n=Pn(i,r);e=Un(e,i,n,t),i=r})})}),tr=(u(function(r,n,t,u){return pn(n,u,r.a9,r.bl,r.bj,function(u,n){var e=r.X&&r.X(u),i=r.bm,o=_n.title,f=_n.body,a=nr(f);return ur(n,function(n){Cn=e;var r=i(n),t=On("body")(A)(r.a_),n=Pn(a,t);f=Un(f,a,n,u),a=t,Cn=0,o!==r.bk&&(_n.title=o=r.bk)})})}),"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function ur(t,u){u(t);var e=0;function i(){e=1===e?0:(tr(i),u(t),1)}return function(n,r){t=n,r?(u(t),2===e&&(e=1)):(0===e&&tr(i),e=2)}}f(function(n,r){return b(Kt,kt,{$:2,b:function(){r&&history.go(r),n()},c:null})}),f(function(n,r){return b(Kt,kt,{$:2,b:function(){history.pushState({},"",r),n()},c:null})}),f(function(n,r){return b(Kt,kt,{$:2,b:function(){history.replaceState({},"",r),n()},c:null})});var er={addEventListener:function(){},removeEventListener:function(){}},ir="undefined"!=typeof window?window:er;t(function(t,u,e){return sn({$:2,b:function(n){function r(n){bn(e(n))}return t.addEventListener(u,r,Tn&&{passive:!0}),function(){t.removeEventListener(u,r)}},c:null})}),f(function(n,r){r=V(n,r);return ot(r)?xr(r.a):Cr});function or(t,u){return{$:2,b:function(r){tr(function(){var n=document.getElementById(t);r(n?{$:0,a:u(n)}:{$:1,a:bt(t)})})},c:null}}f(function(r,n){return or(n,function(n){return n[r](),p})});f(function(n,r){return t=function(){return ir.scroll(n,r),p},{$:2,b:function(n){tr(function(){n({$:0,a:t()})})},c:null};var t});t(function(n,r,t){return or(n,function(n){return n.scrollLeft=r,n.scrollTop=t,p})});f(function(n,r){return n&r}),f(function(n,r){return n|r}),f(function(n,r){return n^r});f(function(n,r){return r<<n}),f(function(n,r){return r>>n}),f(function(n,r){return r>>>n});function fr(n){return b(Lr,"\n    ",b(Mr,"\n",n))}function ar(n){return s(Sr,f(function(n,r){return r+1}),0,n)}function cr(n){return 97<=(n=Jr(n))&&n<=122}function vr(n){return(n=Jr(n))<=90&&65<=n}function br(n){return(n=Jr(n))<=57&&48<=n}function sr(n){return cr(n)||vr(n)||br(n)}var lr=1,hr=2,dr=0,$r=w,gr=t(function(n,r,t){for(;;){if(-2===t.$)return r;var u=t.d,e=n,i=s(n,t.b,t.c,s(gr,n,r,t.e));n=e,r=i,t=u}}),pr=function(n){return s(gr,t(function(n,r,t){return b($r,{a:n,b:r},t)}),A,n)},mr=E,yr=(t(function(t,n,r){var u=r.c,r=r.d,e=f(function(n,r){return s(mr,n.$?t:e,r,n.a)});return s(mr,e,s(mr,t,n,r),u)}),function(n){return{$:1,a:n}}),Ar=f(function(n,r){return{$:3,a:n,b:r}}),jr=f(function(n,r){return{$:0,a:n,b:r}}),wr=f(function(n,r){return{$:1,a:n,b:r}}),kr=function(n){return{$:0,a:n}},Nr=function(n){return{$:2,a:n}},xr=function(n){return{$:0,a:n}},Cr={$:1},_r=X,Er=fn,Or=H,Lr=f(function(n,r){return b(I,n,N(r))}),Mr=f(function(n,r){return k(b(R,n,r))}),Sr=t(function(n,r,t){for(;;){if(!t.b)return r;var u=t.b,e=n,i=b(n,t.a,r);n=e,r=i,t=u}}),Tr=x,zr=t(function(n,r,t){for(;;){if(1<=g(n,r))return t;var u=n,e=r-1,i=b($r,r,t);n=u,r=e,t=i}}),qr=f(function(n,r){return s(zr,n,r,A)}),Fr=f(function(n,r){return s(Tr,n,b(qr,0,ar(r)-1),r)}),Jr=function(n){var r=n.charCodeAt(0);return r<55296||56319<r?r:1024*(r-55296)+n.charCodeAt(1)-56320+65536},Br=function(n){return s(Sr,$r,A,n)},Dr=function(n){var r=n.charCodeAt(0);return isNaN(r)?Cr:xr(r<55296||56319<r?{a:n[0],b:n.slice(1)}:{a:n[0]+n[1],b:n.slice(2)})},Pr=f(function(n,r){return"\n\n("+Or(n+1)+(") "+fr(Rr(r)))}),Rr=function(n){return b(Ir,n,A)},Ir=f(function(n,r){for(;;)switch(n.$){case 0:var t=n.a,u=n.b,e=function(){var n=Dr(t);if(1===n.$)return!1;var r=n.a,n=r.b;return function(n){return cr(n)||vr(n)}(r.a)&&b(_r,sr,n)}(),i=u,e=b($r,e?"."+t:"['"+t+"']",r);n=i,r=e;continue;case 1:var u=n.b,o="["+Or(n.a)+"]",i=u,e=b($r,o,r);n=i,r=e;continue;case 2:var f=n.a;if(f.b){if(f.b.b){var a=(r.b?"The Json.Decode.oneOf at json"+b(Lr,"",Br(r)):"Json.Decode.oneOf")+" failed in the following "+Or(ar(f))+" ways:";return b(Lr,"\n\n",b($r,a,b(Fr,Pr,f)))}n=i=u=f.a,r=e=r;continue}return"Ran into a Json.Decode.oneOf with no possibilities"+(r.b?" at json"+b(Lr,"",Br(r)):"!");default:o=n.a,f=n.b;return(a=r.b?"Problem with the value at json"+b(Lr,"",Br(r))+":\n\n    ":"Problem with the given value:\n\n")+(fr(b(Er,4,f))+"\n\n")+o}}),Wr=u(function(n,r,t,u){return{$:0,a:n,b:r,c:t,d:u}}),Xr=[],Yr=z,Zr=f(function(n,r){return F(r)/F(n)}),Gr=Yr(b(Zr,2,32)),Hr=l(Wr,0,Gr,Xr,Xr),Kr=C,Qr=(f(function(n,r){return n(r)}),f(function(n,r){return r(n)}),q),Ur=function(n){return n.length},Vr=f(function(n,r){return 0<g(n,r)?n:r}),nt=_,rt=f(function(n,r){for(;;){var t=b(nt,32,n),u=t.b,t=b($r,{$:0,a:t.a},r);if(!u.b)return Br(t);n=u,r=t}}),tt=f(function(n,r){for(;;){var t=Yr(r/32);if(1===t)return b(nt,32,n).a;n=b(rt,n,A),r=t}}),ut=f(function(n,r){if(r.e){var t=32*r.e,u=Qr(b(Zr,32,t-1)),n=n?Br(r.h):r.h,n=b(tt,n,r.e);return l(Wr,Ur(r.g)+t,b(Vr,5,u*Gr),n,r.g)}return l(Wr,Ur(r.g),Gr,Xr,r.g)}),et=e(function(n,r,t,u,e){for(;;){if(r<0)return b(ut,!1,{h:u,e:t/32|0,g:e});var i={$:1,a:s(Kr,32,r,n)};n=n,r=r-32,t=t,u=b($r,i,u),e=e}}),it=f(function(n,r){if(0<n){var t=n%32,u=s(Kr,t,n-t,r);return v(et,r,n-t-32,n,A,u)}return Hr}),ot=function(n){return!n.$},ft=K,at=Q,ct=function(n){return{$:0,a:n}},vt=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Q=function(n){return n},bt=Q,st=i(function(n,r,t,u,e,i){return{aj:i,am:r,aw:u,ay:t,aE:n,aF:e}}),lt=Y,ht=function(n){return n.length},dt=W,$t=f(function(n,r){return n<1?r:s(dt,n,ht(r),r)}),gt=G,pt=f(function(n,r){return n<1?"":s(dt,0,n,r)}),mt=function(n){for(var r=0,t=n.charCodeAt(0),u=43==t||45==t?1:0,e=u;e<n.length;++e){var i=n.charCodeAt(e);if(i<48||57<i)return Cr;r=10*r+i-48}return e==u?Cr:xr(45==t?-r:r)},yt=e(function(n,r,t,u,e){if(""===e||b(lt,"@",e))return Cr;var i=b(gt,":",e);if(i.b){if(i.b.b)return Cr;var o=i.a,i=mt(b($t,o+1,e));if(1===i.$)return Cr;i=i;return xr(h(st,n,b(pt,o,e),i,r,t,u))}return xr(h(st,n,e,Cr,r,t,u))}),At=u(function(n,r,t,u){if(""===u)return Cr;var e=b(gt,"/",u);if(e.b){e=e.a;return v(yt,n,b($t,e,u),r,t,b(pt,e,u))}return v(yt,n,"/",r,t,u)}),jt=t(function(n,r,t){if(""===t)return Cr;var u=b(gt,"?",t);if(u.b){u=u.a;return l(At,n,xr(b($t,u+1,t)),r,b(pt,u,t))}return l(At,n,Cr,r,t)}),wt=(f(function(n,r){if(""===r)return Cr;var t=b(gt,"#",r);if(t.b){t=t.a;return s(jt,n,xr(b($t,t+1,r)),b(pt,t,r))}return s(jt,n,Cr,r)}),Z),kt=function(n){for(;;)0},Nt=an,G=Nt(0),xt=u(function(n,r,t,u){if(u.b){var e=u.a,i=u.b;if(i.b){var o=i.a,f=i.b;if(f.b){u=f.a,i=f.b;if(i.b){f=i.b;return b(n,e,b(n,o,b(n,u,b(n,i.a,500<t?s(Sr,n,r,Br(f)):l(xt,n,r,t+1,f)))))}return b(n,e,b(n,o,b(n,u,r)))}return b(n,e,b(n,o,r))}return b(n,e,r)}return r}),Ct=t(function(n,r,t){return l(xt,n,r,0,t)}),_t=f(function(t,n){return s(Ct,f(function(n,r){return b($r,t(n),r)}),A,n)}),Et=cn,Ot=f(function(r,n){return b(Et,function(n){return Nt(r(n))},n)}),Lt=t(function(t,n,u){return b(Et,function(r){return b(Et,function(n){return Nt(b(t,r,n))},u)},n)}),Mt=yn,St=f(function(n,r){return sn(b(Et,Mt(n),r))}),Z=t(function(n,r,t){return b(Ot,function(n){return 0},(r=b(_t,St(n),r),s(Ct,Lt($r),Nt(A),r)))}),Q=t(function(n,r,t){return Nt(0)}),yn=f(function(n,r){return b(Ot,n,r)});mn.Task={b:G,c:Z,d:Q,e:yn,f:void 0};function Tt(n){return 1-b(ru,n,2)/2+b(ru,n,4)/24-b(ru,n,6)/720+b(ru,n,8)/40320-b(ru,n,10)/3628800+b(ru,n,12)/479001600}function zt(n){return b(Lr,"",n)}function qt(n){return b(_r,function(n){return"0"===n},zt(b(uu,n.ap,k([n.a2]))))?1:n.av<0?2:0}function Ft(n){return n<0?-n:n}function Jt(n){return"0"!==b(cu,1,n)?n:Jt(b(au,1,n))}function Bt(n){return n.b?xr(n.a):Cr}function Dt(n){var r=n.a,t=n.b;return"9"!==r?48<=(r=Jr(r))&&r<57?b(du,$u(r+1),t):"0":1===(n=Dr(t)).$?"01":b(du,"0",Dt(n.a))}function Pt(n){return b(du,n,"")}function Rt(n){return(n=b(Mr,".",n)).b?n.b.b?{a:n.a,b:n.b.a}:{a:n.a,b:"0"}:{a:"0",b:"0"}}function It(n){function r(n){return 2<ht(n)?b($r,b(cu,2,n),r(b(au,2,n))):ht(n)?k([n]):A}var t=3<ht(n)?b(cu,3,n):n;return Br(b($r,t,r(b(au,3,n))))}function Wt(n){function r(n){return 3<ht(n)?b($r,b(cu,3,n),r(b(au,3,n))):k([n])}return Br(r(n))}function Xt(n){return b(qu,"click",ct(n))}function Yt(n){return n-b(ru,n,3)/6+b(ru,n,5)/120-b(ru,n,7)/5040+b(ru,n,9)/362880-b(ru,n,11)/39916800}var Zt,Gt,Ht=An("Task"),Kt=f(function(n,r){return Ht(b(Ot,n,r))}),Qt=jn(A),Ut=jn(A),yn=f(function(n,r){switch(n){case 0:return r+1;case 1:return r+5;case 2:return r-1;default:return r-5}}),Vt=On("button"),nu=M,ru=L,tu=e(function(n,r,t,u,e){return{a2:t,ap:r,av:n,M:u,N:e}}),uu=f(function(n,r){return r.b?s(Ct,$r,r,n):n}),eu=D,iu=t(function(n,r,t){return 0<n?s(iu,n>>1,y(r,r),1&n?y(t,r):t):t}),ou=f(function(n,r){return s(iu,n,r,"")}),fu=f(function(n,r){var t=ht(r),t=g(t,n)<0?Ft(n-t):0;return y(r,b(ou,t,"0"))}),au=f(function(n,r){return n<1?r:s(dt,0,-n,r)}),cu=f(function(n,r){return n<1?"":s(dt,-n,ht(r),r)}),vu=f(function(n,r){var t=n.a2;switch(t.$){case 1:return Jt(r);case 2:return r;default:return b(fu,t.a,r)}}),bu=H,su=f(function(n,r){for(;;){if(!r.b)return!1;var t=r.b;if(n(r.a))return!0;n=n,r=t}}),lu=P,hu=f(function(n,r){var t=b(su,function(n){return"0"!==n&&"."!==n},s(lu,$r,A,r));return y(n&&t?"-":"",r)}),du=B,$u=function(n){return n<0||1114111<n?"�":65535<n?String.fromCharCode(55296+Math.floor((n-=65536)/1024),n%1024+56320):String.fromCharCode(n)},gu=function(n){return n===1/0||n===-1/0},pu=J,mu=f(function(n,r){return r.$?Cr:xr(n(r.a))}),yu=t(function(n,r,t){return y(t,b(ou,n-ht(t),Pt(r)))}),Au=function(n){for(var r=n.length,t=Array(r),u=0;u<r;){var e=n.charCodeAt(u);e<55296||56319<e?t[r-u]=n[u]:(t[r-u]=n[u+1],t[r-++u]=n[u-1]),u++}return t.join("")},ju=f(function(n,r){var t=r.b;return{a:n(r.a),b:t}}),wu=f(function(n,r){return r.$?n:r.a}),ku=t(function(n,r,t){if(gu(t)||pu(t))return bu(t);var u=t<0,e=Rt(function(n){var r=b(Mr,"e",bu(Ft(n)));if(r.b){if(r.b.b){var t=r.a,u=r.b.a,e=b(wu,0,mt(b(wt,"+",u)?b($t,1,u):u)),u=Rt(t),u=y(u.a,u.b),u=e<0?b(wu,"0",b(mu,function(n){return n.a+"."+n.b},b(mu,ju(Pt),Dr(y(b(ou,Ft(e),"0"),u))))):s(yu,e+1,"0",u);return y(n<0?"-":"",u)}return y(n<0?"-":"",t=r.a)}return""}(Ft(t))),i=e.a,o=e.b,f=ht(i)+r,t=y(b(ou,1-f,"0"),s(yu,f,"0",y(i,o))),e=ht(t),f=b(Vr,1,f),e=b(n,u,s(dt,f,e,t)),t=s(dt,0,f,t),e=e?Au(b(wu,"1",b(mu,Dt,Dr(Au(t))))):t,t=ht(e),r="0"===e?e:0<r?g(r,ht(o))<0?s(dt,0,t-r,e)+("."+s(dt,t-r,t,e)):y(i+".",s(yu,r,"0",o)):y(e,b(ou,Ft(r),"0"));return b(hu,u,r)})(f(function(n,r){var t=Dr(r);if(1===t.$)return!1;if("5"!==t.a.a)return 53<(r=Jr(t.a.a))&&n||53<=r&&!n;if(""===t.a.b)return!n;return!0})),Nu=f(function(r,n){var t=function(){var n=r.a2;switch(n.$){case 1:return ku(n.a);case 0:return bu;default:return ku(n.a)}}(),t=b(Mr,".",t(n)),n=(n=function(n){if(n.b)return xr(n.b);return Cr}(t)).$?"":b(wu,"",Bt(n.a));return{a:b(wu,"",Bt(t)),b:n}}),xu=f(function(n,r){return(n?It:Wt)(b(eu,br,r))}),Cu=f(function(n,r){var t=b(Nu,n,r),u=b(xu,n.O,b(eu,br,t.a)),t=b(vu,n,t.b),e=v(tu,r,u,t,"","");switch(qt(e)){case 2:return m(e,{M:n.S,N:n.T});case 0:return m(e,{M:n.az,N:n.aA});default:return m(e,{M:n.aX,N:n.aY})}}),_u=f(function(n,r){return""===r?"":y(n.w,r)}),Eu=f(function(n,r){var t=_u(n),n=b(Lr,n.u,r.ap),t=t(r.a2);return zt(k([r.M,n,t,r.N]))}),Ou=f(function(n,r){return b(Eu,n,b(Cu,n,r))}),Lu=m({w:".",a2:{$:0,a:0},S:"−",T:"",az:"",aA:"",O:0,u:"",aX:"",aY:""},{w:",",a2:{$:2,a:3},u:" "}),Mu=f(function(n,r){return b(Ou,m(Lu,{a2:{$:2,a:n}}),r)}),Su=On("div"),Tu=On("h1"),zu=Ln,qu=f(function(n,r){return b(zu,n,{$:0,a:r})}),Fu=On("p"),Ju=S,Bu=Mn,Du=On("sup"),Pu=T,Ru=En,Iu=3.141592653589793,yn=(Zt={a9:45,bl:yn,bm:function(n){var r=Iu*n/180,t=k([b(Bu,"font-size","65%"),b(Bu,"font-weight","bold")]);return b(Su,k([b(Bu,"margin","5%"),b(Bu,"font-size","125%")]),k([b(Tu,k([b(Bu,"font-family","Helvetica Neue"),b(Bu,"color","red")]),k([Ru("Power series of SIN,  COS and TAN")])),b(Vt,k([Xt(0)]),k([Ru("+1")])),b(Vt,k([Xt(1)]),k([Ru("+5")])),b(Su,A,k([Ru(Or(n)+"° angle")])),b(Vt,k([Xt(2)]),k([Ru("-1")])),b(Vt,k([Xt(3)]),k([Ru("-5")])),b(Fu,A,k([Ru("sin "+Or(n)+("° = "+b(Mu,12,Ju(r))))])),b(Fu,A,k([Ru("cos "+Or(n)+("° = "+b(Mu,12,nu(r))))])),b(Fu,A,k([Ru("tan "+Or(n)+("° = "+b(Mu,12,Pu(r))))])),b(Fu,A,k([Ru("x = "+Or(n)+("° = "+b(Mu,6,r))+" radians")])),b(Fu,A,k([Ru("sin x ≅ x - x³ / 3! + x⁵ / 5! - x⁷ / 7! + x⁹ / 9! - x"),b(Du,t,k([Ru("11")])),Ru(" /11! = "+b(Mu,12,Yt(r)))])),b(Fu,A,k([Ru("cos x ≅ 1 - x² / 2! + x⁴ / 4! - x⁶ / 6! + x⁸ / 8! - x"),b(Du,t,k([Ru("10")])),Ru(" /10! + x"),b(Du,t,k([Ru("12")])),Ru(" /12! = "+b(Mu,12,Tt(r)))])),b(Fu,A,A),Ru("tan x = sin x / cos x ≅ "+b(Mu,10,Yt(r)/Tt(r)))]))}},rr({a9:function(n){return{a:Zt.a9,b:Qt}},bj:function(n){return Ut},bl:f(function(n,r){return{a:b(Zt.bl,n,r),b:Qt}}),bm:Zt.bm}));Gt={Main:{init:yn(ct(0))(0)}},n.Elm?function n(r,t){for(var u in t)u in r?"init"==u?O(6):n(r[u],t[u]):r[u]=t[u]}(n.Elm,Gt):n.Elm=Gt}(this);