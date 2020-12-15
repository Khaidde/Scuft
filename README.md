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
        position [1, 2, 3] //[x, y, z]
        velocity [4, 5, 6]
        force    [7, 8, 9]
    }
} => step 100 //Run 100 steps of the simulation
```
#### Option 2:
```
PARTICLE p
p.vel[1, 2, 3] //[x, y, z]
p.pos[4, 5, 6]  
p.force[7, 8, 9] 
Start
Step (100)
```
#### Option 3a:
```
fn main = {
    p : Particle = {
        pos = <1, 2, 3>;
        vel = <4, 5, 6>;
        force = <7, 8, 9>;
    }
    step(p, 100);
}
```
#### Option 3b:
```
type DetailedParticle {
    pos : Position(3) = <1, 2, 3>; //<x, y, z>
    vel : Velocity(3) = <4, 5, 6>;
    force : Force(3) =  <7, 8, 9>;

    id = 0;
    name : string = "DetailedParticle"; //name = "DetailedParticle" is also valid
}

fn step : (particle: DetailedParticle, stepCount: number) = {
    particle.pos.x += particle.vel.x * stepCount;
    particle.pos.y += particle.vel.y * stepCount;
    particle.pos.z += particle.vel.z * stepCount;
}

fn main = {
    p1 : Particle;
    step(p1, 100);

    particles : DetailedParticle[1000];

    || step(particles[...], 100); //Iteration: Apply step function on all points in the array, "particles"

    //Define function within the scope of another function
    fn getPositionX : (particle: DetailedParticle) -> number = {
        return particle.pos.x;
    }

    xPosition = getPositionX(p1); //Implicit type "number", equivalent to xPosition : number = getPositionX(p1)
    out(xPosition);

    fn printX : (particle: DetailedParticle) = {
        out(particle.pos.x); //Print "out" to console
    }

    //Iterate over a scope
    || {
        printX(particle[...]);   
        out("-----line divider----");
    }
}
```

## License

This project is licensed under the MIT License.  See the [LICENSE](LICENSE) file for more information.