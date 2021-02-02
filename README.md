# Scuft
SCUFT is an educational scripting language primarily used for physics simulations. It stands for "Scientific Computation
Used For Teaching". Currently, SCUFT has an intended target of intro level college physics and related mathematical subfields. This repository is a prototype project for future development plans.

Note: The typescript implementation of the compiler is discontinued. Currently being ported over to C++ for integration with LLVM.

## Why Use Scuft?
Besides the amazing name, there are a ton of benefits to using Scuft as opposed to many main stream scientific computation languages. The most popular of which include C++ and Fortran. These languages prioritize speed and flexibility, something which is often needed in intense scientific computations but not necessarily in an intro level physics course. 

> "With great power comes a great learning curve" - Anonymous, 2020

Scuft offers a basic scripting language for dealing with these simple scenarios in a more concise format. Instead of drowning in boilerplate, prioritize and [declare](https://en.wikipedia.org/wiki/Declarative_programming) the problem aspects/factors.

tldr: Why not?

## Getting Started

### Setup
Here's a quick guide to setting up the project.

1) Clone the repository

```sh
git clone https://github.com/Khaidde/Scuft
```

2) Install Node and npm if necessary. This project does not currently use Node.JS, but it does use npm which comes bundled with Node. [Download Link](https://nodejs.org/en/download/)

3) Download dev dependencies in cloned repository
```sh
$ npm install
```

4) Build and run project in development mode

```sh
$ npm run build
$ npm run serve
```

5) Live webpage is accessible at `localhost:8100`

### Proposed Syntax (Due to Change with transition to C++)
```
ListItem => type {
    entry: num;
    nextItem: ListItem;
}

operator + => (a: ListItem, b: ListItem) -> ListItem {
    resList = ListItem.{};
    resList.entry = a.entry + b.entry;
    if a == null || b == null {
        resList.nextItem = null;
    } else {
        resList.nextItem = a.nextVec + b.nextVec;
    }
    return resList;
}

Tree => type {
    branch: <Tree..?>;
    entry: num;
}

vector => module {
    Vec3 => type {
        x: num;
        y: num;
        z: num;
    }

    operator dot => (a: Vec3, b: Vec3) -> Vec3 {
        newVec3 = Vec3.{};
        newVec3.x = a.x * b.x;
        newVec3.y = a.y * b.y;
        newVec3.z = a.z * b.z;
    }

    scale => num;
    operator * => (scalar: scale, vec: Vec3) -> Vec3 {
        return Vec.{ 
            x <- scalar * vec.x,
            y <- scalar * vec.y,
            z <- scalar * vec.z
        };
    }
}

length = (a: <num..?>) -> num {
    count ~= 0;
    for i in a {
        count ~= count + 1;
    }
    return count;
}

push = (a: <num..?>, b: num) -> <num..?> {
    newVec = <num..(length(a) + 1)>
    for i in #range [0, length(a)) newVec<i> = a<i>
    newVec<length(a)> = b;
}

main = () {
    size ~= 10;
    
    if (rand() < 0.5) {
        size = 13;
    }

    vec: <num..size>;

    tuple: <<num, num, <string, num>>...10>
}
```

## License

This project is licensed under the MIT License.  See the [LICENSE](LICENSE) file for more information.
