#!/usr/bin/env node
var K=require('/data/projects/regulex/src/Kit');
var assert=require('assert');

var res=buildRegex(3);
var re=new RegExp(res);

K.log(res);
//testBuildRegex();

function buildRegex(n) {
  var map={},reflect={},i,j,k,to,path;
  for (i=0;i<n;i++) {
    path=map[i]={};
    for (j=0;j<10;j++) {
      to=(i*10+j) % n;
      path[to]=path[to] || '';
      path[to]+=j;
      if (to>i)
        (reflect[to]=reflect[to] || {})[i]=1;
    }
    for (to in path)
      if (path[to].length>1)
        path[to]='['+path[to]+']';
  }
  for (to in reflect)
    reflect[to]=Object.keys(reflect[to]);

  while (--i) {
    var trans=map[i],t={},
        prefix=repeat(trans[i]);
    for (to in trans)
      if (to<i) t[to]=trans[to];

    trans=t;
    for (to in trans)
      trans[to]=seq([prefix,trans[to]]);

    var entrances=reflect[i];
    for (j=entrances.length;j--;) {
      var from=entrances[j];
      path=map[from];
      prefix=path[i];
      for (to in trans)
        path[to]=choice([path[to] || '',seq([prefix,trans[to]])]);
    }
  }

  return '^'+repeat(map[0][0])+'$';

  function seq(a) {
    return {
      type:'seq',
      toString:function () {
        var re=a.join("");
        if (this.repeat)
          re=a.length>1?'('+re+')*':re+'*';
        return re;
      }
    };
  }

  function choice(a) {
    var items=[];
    a.forEach(function (re) {
      if (re.type==='choice')
        items=items.concat(re.items);
      else if (re)
        items.push(re);
    });
    return {
      type:'choice', items:items,
      toString:function () {
        var re=items.join("|");
        if (items.length>1 || this.repeat) re='('+re+')';
        if (this.repeat) re+='*';
        return re;
      }
    };
  }
  function repeat(re) {
    if (typeof re==='string') return re+'*';
    re.repeat=true;
    return re;
  }
}



function testBuildRegex() {
  for (var i=2;i<13;i++) {
    K.log("Building "+i);
    var res=buildRegex(i);
    K.log("Length "+res.length);
    var re=new RegExp(res);
    var j=2;
    K.log("Testing "+i);
    while (j--) {
      var n=(Math.random()*1E5|0)*i;
      n=""+n+n;
      assert.ok(re.test(n));
      assert.ok(re.test(n+(i-1))===false,[n,i,n+(i-1)]);
    }
  }
}


function buildDFA(n) {
  var map={};
  for (var i=0;i<n;i++) {
    for (var j=0;j<10;j++) {
      var to=(i*10+j) % n;
      (map[i]=map[i]||{})[j]=to;
    }
  }
  return map;
}


function DFA(a) {
  return {
    test:function (s) {
      for (var i=0,from=0,l=s.length;i<l;i++) {
        from=a[from][s[i]];
        if (from===undefined) return false;
      }
      return from===0;
    }
  };
}

function testDFA() {
  var divisor=1351216;
  var dfa=DFA(buildDFA(divisor));
  var i=6;
  while (i--) {
    var num=(Math.random()*1e6|0)*divisor;
    num="0"+num+num+num;
    num=""+num+num+num;
    assert.ok(dfa.test(num));
    var j=divisor;
    while ((j=j/2|0)>0) {
      assert.ifError(dfa.test(num+j));
    }
  }
}

