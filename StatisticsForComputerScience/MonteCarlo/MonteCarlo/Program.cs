using Microsoft.SqlServer.Server;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MonteCarlo
{
    public class Program
    {
        

        static void Main(string[] args)
        {
            Console.WriteLine("Simple monte carlo: \n");
            MonteCarlo.SimpleMonteCarlo();
            Console.ReadKey();

            Console.Clear();

            Console.WriteLine("Monte Carlo Variation with RNG and Normal distribution: \n");
            MonteCarlo.MonteCarlo_Variation();
            Console.ReadKey();

            Console.Clear();

            for(int i = 0; i< 4; i++)
            {
                Console.WriteLine($"\nTrial {i}: Finding max error and ideal n: ");
                double alpha = 0.99;
                double e = 0.01;
                string maxerror = MonteCarlo.FindMaxError(e, alpha);
                Console.WriteLine(maxerror);
                Console.ReadKey();
            }
            
            
        }
        

        
    }
}
