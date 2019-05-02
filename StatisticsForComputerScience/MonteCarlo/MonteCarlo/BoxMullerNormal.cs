using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MonteCarlo
{
    public class BoxMullerNormal : MonteCarlo
    {
        private MathNet.Numerics.Distributions.Normal normal;

        public BoxMullerNormal(double mean = 0, double std = 1.0)
        {
            normal = new MathNet.Numerics.Distributions.Normal(mean, std);
        }

        public override dynamic GetRandom()
        {
            return normal.Sample();
        }

        
    }
}
