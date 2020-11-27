# Scuft
SCUFT is an educational scripting language primarily used for physics simulations. It stands for "Scientific Computation
Used For Teaching". Currently, SCUFT has an intended target of intro level college physics and related mathematical subfields. This repository is a prototype project for future development plans.

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

### Prototype Syntax (Due to Change)
#### Option 1:
```
//Entry point to declarations 
main {

    //Initialize a particle with phsyical properties
    particle {
        position [x, y, z]
        velocity [x, y, z]
        force    [x, y, z]
    }
} => step 100 //Run 100 steps of the simulation
```
#### Option 2:
```
PARTICLE p
p.vel[x, y, z], p.pos[x, y, z], p.force 
Start
Step (100)
```

## License

This project is licensed under the MIT License.  See the [LICENSE](LICENSE) file for more information.