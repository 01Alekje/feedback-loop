from openai import OpenAI
import os
import json
import sys

client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

with open(sys.argv[1], 'r') as file:
    agda_file = file.read()

with open(sys.argv[2], 'r') as file:
    context_file = file.read()

with open(sys.argv[3], 'r') as file:
    prev_responses_file = file.read()

my_schema = {
    "name": "my_response_format",
    "strict": True,
    "schema": {
        "type": "object",
        "additionalProperties": False,
        "properties": {
        "reply": {
            "description": "The command to execute.",
            "type": "object",
            "anyOf": [
            {
                "title": "GIVE",
                "type": "object",
                "properties": {
                    "command": { "type": "string", "const": "GIVE" },
                    "hole": { "type": "integer" },
                    "expression": { "type": "string" }
                },
                "required": ["command", "hole", "expression"],
                "additionalProperties": False
            },
            {
                "title": "AUTO",
                "type": "object",
                "properties": {
                "command": { "type": "string", "const": "AUTO" },
                "hole": { "type": "integer" }
                },
                "required": ["command", "hole"],
                "additionalProperties": False
            },
            {
                "title": "ADD_BINDERS",
                "type": "object",
                "properties": {
                "command": { "type": "string", "const": "ADD_BINDERS" },
                "binders": {
                    "type": "array",
                    "items": { "type": "string" }
                },
                "function": { "type": "string" }
                },
                "required": ["command", "binders", "function"],
                "additionalProperties": False
            },
            {
                "title": "CASE_SPLIT",
                "type": "object",
                "properties": {
                "command": { "type": "string", "const": "CASE_SPLIT" },
                "hole": { "type": "integer" },
                "binder": { "type": "string" }
                },
                "required": ["command", "hole", "binder"],
                "additionalProperties": False
            }
            ]
        }
        },
        "required": ["reply"]
    }
}

# Just pass your schema directly
def get_structured_output(code: str, context: str, prev_responses):
    completion = client.chat.completions.create(
        model="gpt-5-nano",
        messages=[
            {"role": "system", "content": 
            """You are currently within a feedback loop. You will be given a prompt and you must respond with a JSON object that conforms to the provided schema. Only respond with the JSON object, nothing else.
               Your task will be to try and fill all the holes in the provided code snippet.

               The holes are represented by {!n!} where n is the hole number. For example {!0!} refers to the first hole.
               You will also be given the context of each hole from each prompt. This context will include variables and their types and the goal of the hole (type of expression to be filled).

               To each prompt, you will have the ability to send exactly one of the following commands:
                - AUTO: This command will ask agda to automatically fill the hole with a suitable expression. This should almost always be your first choice. If you use this command, you must provide the hole number that you want filled. This command might fail and will then return you a new prompt with the same hole. Should only be used once for the same hole.
                - GIVE: This command will ask you to provide an expression to fill the hole. You must provide the hole number and the expression you want to use. Do not use newline characters in your expression. This should be used as refinement, for instance if you want to prove Q and you have pq : P -> Q, then you can use GIVE (hole, "pq ?") to fill the hole. Which will create a new hole with the type P. This should not be used to pattern match on anything, use ADD_BINDERS together with CASE_SPLIT for that purpose.
                - ADD_BINDERS: This command will ask you to add a list of binders to a specified function. You must provide the list of binders and the function you want to add them to. If you need to prove something of type P -> Q, you should almost always use this command to bind a identifier to P. This does not create or fill any holes.
                - CASE_SPLIT: Use this command to case split on a binder. This works for binders of algebraic data types (given by the data ... keyword). This is your only method of pattern matching (you cannot use GIVE for this). You must provide the hole number and the binder you want to case split on.

                Tips: 
                - Always try to use AUTO first, it is often able to fill in simple expressions.
                - If you need to prove something of type P -> Q (or in general A -> B -> C -> D), use ADD_BINDERS to add a binder for P to the function you are trying to prove.
                - If you need to case split on a binder, use CASE_SPLIT with the appropriate hole number and binder name. Never use
            """},
            {"role" : "system", "content" : prev_responses},
            {"role" : "system", "content" : code},
            {"role" : "system", "content" : context},
            {"role": "user", "content": "Please try to fill all of the holes in the code given:"}
        ],
        response_format={
            "type": "json_schema",
            "json_schema": my_schema
        }
    )
    
    return json.loads(completion.choices[0].message.content)

result = get_structured_output(agda_file, context_file, prev_responses_file)
print(json.dumps(result), end='')