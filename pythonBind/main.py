import sys
import os

# path to the compiled sciantixModule 
module_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'build', 'python'))
if module_path not in sys.path:
    # add the module to the system path
    sys.path.append(module_path)

# importation of the Module
import sciantixModule


def main():
    # Creation of a Model 
    model = sciantixModule.Model()
    print(model)

    # Creation of an entity
    entity = sciantixModule.Entity()
    entity.setReference("entityyyy")
    print(f"Entity reference: {entity.getReference()}")

    # Creation of  a material 
    material = sciantixModule.Material()
    # Checking the if material is a instance of an Entity
    print(f"Material is an Entity: {isinstance(material, sciantixModule.Entity)}")
    material.setReference("Materailllll")
    print(f"mat ref : {material.getReference()}")

if __name__ == "__main__":
    main()
