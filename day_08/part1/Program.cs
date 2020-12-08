using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

namespace Part1
{
    class Program
    {
        static int program_counter = 0;
        static long accumulator = 0;

        

        static void Main(string[] args)
        {
            var input = File.ReadAllLines(@"C:\projects\advent_of_code\data\day_08\example_part1.txt").ToList();
            string output;
            var op_codes = input.Select(x => x.Split(" ",StringSplitOptions.TrimEntries)[0]).ToArray();
            var operands = input.Select(x => x.Split(" ",StringSplitOptions.TrimEntries)[1]).ToArray();

            var program_counter_history = new List<int>();
            
            do 
            {
                program_counter_history.Add(program_counter);
                Console.Write($"{program_counter} : ");
                switch(op_codes[program_counter])
                {
                    case "nop":
                        output = nop(operands[program_counter]);
                        break;
                    case "acc":
                        output = acc(operands[program_counter]);
                        break;
                    case "jmp":
                        output = jmp(operands[program_counter]);
                        break;
                    default:
                        throw new Exception($"Unrecognized op code, '{op_codes[program_counter]}'");
                }
                Console.WriteLine(output);
            }
            while((!program_counter_history.Contains(program_counter)));
            Console.WriteLine($"program staring infinite loop at line {program_counter}: '{input[program_counter]}', acc = {accumulator}");
        }


        static string nop(string operand)
        {
            program_counter += 1;
            return $"nop {operand}, acc={accumulator}";
        }

        static string acc(string operand)
        {
            program_counter += 1;
            var x = Int64.Parse(operand);
            accumulator += x;
            return $"acc {x}, acc={accumulator}";
        }

        static string jmp(string operand)
        {
            var x = int.Parse(operand);
            program_counter += x;
            return $"jmp {x}, acc={accumulator}";
        }


    }
}
